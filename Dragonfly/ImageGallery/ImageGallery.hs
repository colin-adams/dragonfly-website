module Dragonfly.ImageGallery.ImageGallery (
                                            Gallery (..),
                                            authorizedUploadGalleries,
                                            allGalleries,
                                            divImageGallery,
                                            handleImageGallery,
                                            handleImages,
                                            imageDirectory,
                                            tempDirectory,
                                            displayPreview,
                                            exifData,
                                            UploadData(..),
                                            displayPreviewPage,
                                            saveFiles
                                           ) where

import Control.Monad.Reader
import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy.UTF8 as LU
import Data.List (nub)
import qualified Data.Map as Map
import Data.Ratio
import Data.Tree

import qualified Database.HaskellDB as DB
import Database.HaskellDB.Database as DB
import qualified Database.GalleryTable as GT
import qualified Database.GalleryImageTable as GIT
import qualified Database.ImageTable as IT

import Graphics.Exif
import Graphics.GD

import Happstack.Server.SimpleHTTP
import Happstack.Server.HTTP.FileServe

import Numeric

import System.Environment
import System.Time

import qualified Text.XHtml.Strict.Formlets as F
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((<<), (+++)) 
import Text.Pandoc.Shared
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Dragonfly.ApplicationState
import qualified Dragonfly.Authorization.Authorities as Auth
import Dragonfly.URISpace (imageGalleryURL)
import qualified Dragonfly.Authorization.User as U
import Dragonfly.ImageGallery.Exif
import Dragonfly.Forms

-- | File-system directory where uploaded images are stored
imageDirectory :: IO String
imageDirectory = do
  h <- homeDirectory
  return $ h ++ "/dragonfly-website/files/images/"

-- | Home directory of user running website
homeDirectory :: IO String
homeDirectory = getEnv "HOME"

-- | File-system directory where temporary images are stored
tempDirectory :: String
tempDirectory = "/tmp/"

data Gallery = Gallery {
      name :: String,
      parent :: Maybe String,
      readCapabilityName :: String,
      uploadCapabilityName :: String,
      administrationCapabilityName :: String
    } deriving Show

-- | Display preview picture and EXIF information
displayPreview :: String -> String -> String -> String -> [(String, String)] -> X.Html
displayPreview caption description previewName originalName exif =
    let doc = readMarkdown defaultParserState description
        desc = writeHtml defaultWriterOptions doc
    in (X.h1 << X.stringToHtml caption) 
       +++ X.thediv << desc +++ X.image X.! [X.src previewName]
       +++ exifDiv exif
       +++ X.thediv << X.anchor X.! [X.href originalName] << "original"

-- | All interpretable EXIF data for fname                            
-- currently excludes MakerNote
exifData :: Bool -> String -> IO [(String, String)]
exifData isTemp fname = do
  iDir <- imageDirectory
  let dir = if isTemp
            then tempDirectory
            else iDir
  exif <- fromFile (dir ++ fname)
  allTags exif

-- | Display selected EXIF data as XHtml
exifDiv :: [(String, String)] -> X.Html
exifDiv tags = 
    let mnu = lookup manufacturer tags
        mdl = lookup model tags
        fL = lookup focalLength tags
        dtO = lookup dtOriginal tags
        fN = lookup fNumber tags
        exp = lookup exposure tags
        speed = lookup iso tags
        flsh  = lookup flash tags
        mtr = lookup meter tags
        expM = lookup exposureMode tags
        col = lookup colourSpace tags
        wB = lookup whiteBalance tags
        usr = lookup userComment tags
        latR = lookup gpsLatRef tags
        lat = lookup gpsLat tags
        longR = lookup gpsLongRef tags
        long = lookup gpsLong tags
        altR = lookup gpsAltRef tags
        alt = lookup gpsAlt tags
        pairs = [(makeI, mnu), (modelI, mdl), (focalLengthI, fL), 
                 (dtOriginalI, dtO), (fNumberI, fN), (exposureI, exp),
                 (isoI, speed), (flashI, flsh), (meterI, mtr),
                 (exposureModeI, expM), (colourSpaceI, col),
                 (whiteBalanceI, wB), (userCommentI, usr),
                 (gpsLatRefI, latR), (gpsLatI, lat),
                 (gpsLongRefI, longR), (gpsLongI, long),
                 (gpsAltRefI, altR), (gpsAltI, alt)
                ]
    in X.thediv << tagTable pairs
   
-- | XHtml table of Exif information
tagTable :: [(String, Maybe String)] -> X.Html
tagTable tags = 
    X.table << X.tbody << map tagDisplay tags

-- | XHtml fragment to display a tag    
tagDisplay :: (String, Maybe String) -> X.Html
tagDisplay (tag, value) =
    case value of
      Nothing -> X.noHtml
      Just v -> X.tr << ((X.td << X.stringToHtml tag) +++ (X.td << X.stringToHtml v))

-- | All galleries in database
allGalleries :: Database -> IO [Gallery]
allGalleries db = do
  rs <- DB.query db (DB.table GT.galleryTable)
  return $ map newGallery rs

-- | All galleries names to which the user can upload images
authorizedUploadGalleries :: U.User -> Database -> IO [String]
authorizedUploadGalleries user db = do
  rs <- DB.query db (DB.table GT.galleryTable)
  let frs = filter (U.authorizedTo user . (DB.! GT.uploadImageCapabilityName)) rs
  return $ map (DB.! GT.galleryName) frs

-- | Html div to invoke image gallery
divImageGallery :: X.Html
divImageGallery = X.thediv << (X.anchor X.! [X.href imageGalleryURL] << "Image gallery")

-- | Handler for images
handleImages :: MyServerPartT Response 
handleImages = do
  iDir <- liftIO imageDirectory
  rq <- askRq
  let paths = rqPaths rq
  if null paths
    then mzero 
    else fileServeStrict [] iDir

-- | Handler for imageGalleryURL
handleImageGallery :: MyServerPartT Response 
handleImageGallery = dir (tail imageGalleryURL) $ do
  rq <- askRq
  let cookies = rqCookies rq
      sc = lookup sessionCookie cookies
      params = rqInputs rq
      galleryParam = lookup galleryParameter params
      previewParam = lookup previewParameter params
  case previewParam of
       Just p -> displayPreviewPicture $ LU.toString $ inputValue p
       Nothing -> displayGalleryTree galleryParam sc

-- | Display a gallery tree and immedaite child pictures
displayGalleryTree :: Maybe Input -> Maybe Cookie -> MyServerPartT Response
displayGalleryTree galleryParam sc = do
  ApplicationState db sessions <- lift ask
  sess <- liftIO $ readMVar sessions
  (header, galleries, pictures) <- 
      case galleryParam of
        Nothing -> liftIO $ 
                  do
                    gs <-  topLevelGalleries db
                    return ("Image galleries", gs, [])
        Just g -> liftIO $
                 do
                   let gName = LU.toString $ inputValue g
                   gs <- childGalleries db gName
                   pics <- images db gName
                   return (gName, gs, pics)
  authorizedGalleries <- liftIO $ filterM (isGalleryAuthorized sc sess) galleries
  authorizedGalleryHeadlines <- liftIO $ mapM (readHeadline db) authorizedGalleries
  galDiv <- liftIO $ galleriesDiv header authorizedGalleryHeadlines db pictures
  ok $ toResponse $ galleryHeader +++ (X.body << galDiv)

-- | Html head for galleries
galleryHeader :: X.Html
galleryHeader = X.header <<
                (X.style X.! [X.thetype "text/css"]
                  << "@import url(/styles/image_gallery.css);")

-- | Gathered form data
data UploadData = UploadData { 
      caption :: String,
      galleryNames :: [String],
      imageFile :: F.File,
      description :: String,
      previousFileName :: String,
      previousCT :: String
                             } deriving Show

-- TODO: -- previewTable should have a unique index
-- | Show preview version of preview, with EXIF and link to original
displayPreviewPicture :: String -> MyServerPartT Response
displayPreviewPicture previewName = do
  ApplicationState db _ <- ask
  details <- liftIO $ pictureDetailsFromPreview previewName db
  case details of
    Nothing -> notFound $ toResponse (previewName ++ " was not found")
    Just (thumbnail, original, caption, description, _uploadTime) -> do
        exif <- liftIO $ exifData False original
        okHtml $ displayPreview caption description previewName original exif

pictureDetailsFromPreview :: String -> Database -> IO (Maybe (String, String, String, String, CalendarTime))
pictureDetailsFromPreview previewName db = do
  let q = do 
        t <- DB.table IT.imageTable
        DB.restrict (t DB.! IT.preview DB..==. DB.constant previewName)
        DB.project (IT.thumbnail DB.<<  t DB.! IT.thumbnail DB.# 
                    IT.original DB.<<  t DB.! IT.original DB.# 
                    IT.caption DB.<<  t DB.! IT.caption DB.# 
                    IT.body DB.<<  t DB.! IT.body DB.# 
                    IT.uploadTime DB.<<  t DB.! IT.uploadTime
                   )
  rs <- DB.query db q
  if null rs
     then return Nothing
     else return $ Just (head rs DB.! IT.thumbnail, head rs DB.! IT.original, head rs DB.! IT.caption,  
                         head rs DB.! IT.body,  head rs DB.! IT.uploadTime)  

-- | Display preview image and options to confirm or change
displayPreviewPage :: UploadData -> String -> (String, String, String) -> String ->
                     F.Env -> XForm UploadData -> MyServerPartT Response
displayPreviewPage udata dir fnames@(thumbnailName, previewName, fname) imageType env frm = do
  liftIO $ saveFiles dir fnames imageType
  exif <- liftIO $ exifData True fname
  xhtml <- createPreviewSubmit (displayPreview (caption udata) (description udata) 
                               ("temp/" ++ previewName) ("temp/" ++ fname) exif) (enhancedEnvironment fname imageType env) frm
  okHtml xhtml

-- | Desired width for thumbnails in pixels
widthThumbnail :: Integer
widthThumbnail = 120

-- | Desired width for previews in pixels
widthPreview :: Integer
widthPreview = 640

-- | Computed height for images in pixels
height :: Integer -> Rational -> Int
height width ratio = 
    let answerRatio = (width % 1) * ratio
        answer = fromRat answerRatio :: Double
    in floor answer

-- | Quality of saved JPEG preview images
imageQuality :: Int
imageQuality = 85

-- | Quality of saved JPEG thumbnail images
thumbnailImageQuality :: Int
thumbnailImageQuality = 70

-- | Save image files to disk
saveFiles :: String -> (String, String, String) -> String -> IO ()
saveFiles dir fnames@(thumbnailName, previewName, fname) imageType =
  case imageType of
    "jpeg" -> do -- TODO png and gif
      image <- liftIO $ loadJpegFile (dir ++ fname)
      (w, h) <- liftIO $ imageSize image
      let ratio = fromIntegral h % fromIntegral w
      previewImage <- liftIO $ resizeImage (fromIntegral widthPreview) (height widthPreview ratio) image
      liftIO $ saveJpegFile imageQuality (dir ++ previewName) previewImage
      thumbnailImage <- liftIO $ resizeImage (fromIntegral widthThumbnail)(height widthThumbnail ratio) image
      liftIO $ saveJpegFile thumbnailImageQuality (dir ++ thumbnailName) thumbnailImage

enhancedEnvironment :: String -> String -> F.Env -> F.Env
enhancedEnvironment fname imageType env =
    let count = length env
        prologue = take (count - 2) env 
        fnameKey = fst (env !! (count - 2))
        ctKey = fst (env !! (count - 1))
        fnamePair = (fnameKey, Left fname)
        ctPair = (ctKey, Left imageType)
    in if null imageType
       then env
       else prologue ++ (fnamePair:[ctPair])

-- | Name of URI parameter giving gallery name
galleryParameter :: String
galleryParameter = "gallery"

-- | Name of URI parameter giving preview picture name
previewParameter :: String
previewParameter = "preview"

-- | Gallery name, number of pictures, and if non-zero, latest picture, and upload date
data GalleryHeadline =  GalleryHeadline {gName ::String,             -- ^Name of gallery
                                         count :: Int,               -- ^Number of pictures (including all in sub-galleries)
                                         picture :: Maybe (String, String, String, CalendarTime) -- ^File names and upload time of latest picture 
                                        }

-- | Read in sufficient information to form a gallery headline display
readHeadline :: Database -> Gallery -> IO GalleryHeadline
readHeadline db gallery = do
  -- get the list of all (recursive) child gallery names
  gNames <- unfoldTreeM_BF (childGalleryNames db) (name gallery)
  imageNumbers <- mapM (images db) (Data.Tree.flatten gNames)
  let indices = nub (concat imageNumbers)
  recentUpload <- mostRecentImage db indices
  return $ GalleryHeadline (name gallery) (length indices) recentUpload

-- | Most recent thumbnail-image from list
mostRecentImage :: Database -> [Integer] -> IO (Maybe (String, String, String, CalendarTime))
mostRecentImage db indices =
  if null indices then return Nothing else
      do let q 
                 = do t <- DB.table IT.imageTable
                      DB.restrict (t DB.! IT.indexNumber `DB._in` map DB.constant indices)
                      DB.order [DB.desc t IT.uploadTime]
                      DB.top 1
                      return t
         rs <- DB.query db q
         return $ Just (head rs DB.! IT.thumbnail, head rs DB.! IT.preview, 
                             head rs DB.! IT.original, head rs DB.! IT.uploadTime)

-- | Display headline list of galleries      
galleriesDiv :: String -> [GalleryHeadline] -> Database -> [Integer] -> IO X.Html
galleriesDiv header galleries db pics = do
    let gallsDiv = X.thediv << X.ulist X.! [X.theclass "galleries"] << map displayGallery galleries
    picsDiv <- if null pics
              then return X.noHtml
              else picturesDiv db pics
    return $ X.thediv << (X.h1 << header)
       +++ gallsDiv
       +++ picsDiv

-- | details of a single picture from database
type PictureInfo = (String, -- thumbnail name
                    String, -- preview name
                    String, -- title
                    CalendarTime, -- upload time
                    String -- user name
                   )

-- | HTML div showing first 10 thumbnails in gallery
picturesDiv :: Database -> [Integer] -> IO X.Html
picturesDiv db pics = do
  let dropCount = 0
      maximumThumbnails = 10
      indices = take maximumThumbnails (drop dropCount pics)
  picsInfo <- pictureInfo db indices
  return $ X.thediv << X.ulist X.! [X.theclass "images"] << X.concatHtml (map pictureInfoItem picsInfo)

pictureInfoItem :: PictureInfo -> X.Html
pictureInfoItem (thumbnail, preview, caption, uploadTime, user) =
    X.li << (X.anchor X.! [X.href $ X.stringToHtmlString previewRef] << X.image X.! [X.src thumbnail]) +++
         X.h3 << X.anchor X.! [X.href $ X.stringToHtmlString previewRef] << caption +++
         X.thediv << ("Posted by: " ++ user) +++
         X.thediv << calendarTimeToString uploadTime
    where previewRef = imageGalleryURL ++ "?" ++ previewParameter ++ "=" ++ preview                        

pictureInfo :: Database -> [Integer] -> IO [PictureInfo]
pictureInfo db indices = do
  let q = do
        t <- DB.table IT.imageTable
        DB.restrict (t DB.! IT.indexNumber `DB._in` map DB.constant indices)
        return t
  rs <- DB.query db q
  return $ map newPictureInfo rs 

-- | Contruct a PictureInfo from its database record
-- Signature commented out as it needs a really long context.
--newPictureInfo :: DB.Record vr -> PictureInfo
newPictureInfo rec = (rec DB.! IT.thumbnail, rec DB.! IT.preview, rec DB.! IT.caption,
                      rec DB.! IT.uploadTime, rec DB.! IT.userName)

-- | Display one gallery headline
displayGallery :: GalleryHeadline -> X.Html
displayGallery (GalleryHeadline name count picture) =
 X.li << (X.anchor X.! [X.href $ X.stringToHtmlString $ imageGalleryURL ++ "?" ++ galleryParameter ++ "=" ++ name] << name) +++
  let toBe = case count of
               1 -> "is "
               _ -> "are "
      pictureNoun = case count of
                      1 -> " picture"
                      _ -> " pictures"
      pictureCountPhrase = "There " ++ toBe ++ show count ++ pictureNoun ++ " in this gallery."
  in case count of
       0 -> X.p << "There are no pictures in this gallery"
       _ -> case picture of
             Just (image, preview, original, date) -> 
                 (X.anchor X.! [X.href $ X.stringToHtmlString previewRef] << 
                        (X.image X.! [X.src image])) +++
                 X.p << pictureCountPhrase +++ X.p << updatedPhrase
                 where previewRef = imageGalleryURL ++ "?" ++ previewParameter ++
                                    "=" ++ preview
                       updatedPhrase = "Last updated: " ++ 
                                       calendarTimeToString date ++ " UTC"

-- | Get list of all top-level galleries from database
topLevelGalleries :: Database -> IO [Gallery]
topLevelGalleries db = do
  let q = do
        t <- DB.table GT.galleryTable
        DB.restrict (DB.isNull $ t DB.! GT.parentGalleryName)
        return t
  rs <- DB.query db q
  return $ map newGallery rs

-- | Get list of names of all immediate child galleries of a named gallery
childGalleryNames :: Database -> String -> IO (String, [String])
childGalleryNames db gName = do
  let q = do
        t <- DB.table GT.galleryTable
        DB.restrict (t DB.! GT.parentGalleryName DB..==. DB.constJust gName)
        DB.project (GT.galleryName DB.<<  t DB.! GT.galleryName)
  rs <- DB.query db q
  return (gName, map (DB.! GT.galleryName) rs)

-- | Get list of immediate child galleries of a named gallery
childGalleries :: Database -> String -> IO [Gallery]
childGalleries db gName = do
  let q = do
        t <- DB.table GT.galleryTable
        DB.restrict (t DB.! GT.parentGalleryName DB..==. DB.constJust gName)
        return t
  rs <- DB.query db q
  return $ map newGallery rs

-- | get the (non-recursive) image indexNumbers from a given named gallery
images :: Database -> String -> IO [Integer]
images db gName = do
  let q = do
        t <- DB.table GIT.galleryImageTable
        DB.restrict (t DB.! GIT.galleryName DB..==. DB.constant gName)
        DB.project (GIT.indexNumber DB.<< t DB.! GIT.indexNumber)
  rs <- DB.query db q
  return $ map (DB.! GIT.indexNumber) rs

-- | Contruct a Gallery from its database record
-- Signature commented out as it needs a really long context.
--newGallery :: DB.Record vr -> Gallery
newGallery rec = Gallery (rec DB.! GT.galleryName) (rec DB.! GT.parentGalleryName) (rec DB.! GT.readImageCapabilityName)  (rec DB.! GT.uploadImageCapabilityName)  (rec DB.! GT.administerGalleryCapabilityName) 

-- | Check authorization of user to view gallery
isGalleryAuthorized :: Maybe Cookie -> Sessions -> Gallery -> IO Bool
isGalleryAuthorized cook sessions gallery =
  case cook of
    Nothing -> return defaultAuth
    Just c -> do
              let key = cookieValue c
                  u = session sessions (read key)
              case u of
                Nothing -> return defaultAuth
                Just user -> return (U.authorizedTo user capability || defaultAuth)
    where capability = readCapabilityName gallery 
          defaultAuth = capability == Auth.readGalleryCapabilityName

