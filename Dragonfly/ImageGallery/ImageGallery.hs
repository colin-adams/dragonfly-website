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
                                            exifData
                                           ) where

import Control.Monad.Reader
import Control.Concurrent.MVar

import Data.List (nub)
import qualified Data.Map as Map
import Data.Tree

import qualified Database.HaskellDB as DB
import Database.HaskellDB.Database as DB
import qualified Database.GalleryTable as GT
import qualified Database.GalleryImageTable as GIT
import qualified Database.ImageTable as IT

import Graphics.Exif

import Happstack.Server.SimpleHTTP
import Happstack.Server.HTTP.FileServe

import System.Time

import qualified Text.XHtml.Strict as X
import Text.Pandoc.Shared
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Dragonfly.ApplicationState
import qualified Dragonfly.Authorization.Authorities as Auth
import Dragonfly.URISpace (imageGalleryURL)
import qualified Dragonfly.Authorization.User as U
import Dragonfly.ImageGallery.Exif

-- | File-system directory where uploaded images are stored
imageDirectory :: String
imageDirectory = "/home/colin/dragonfly-website/files/images/"

-- | File-system directory where temporary images are stored
tempDirectory :: String
tempDirectory = "/home/colin/dragonfly-website/files/images/temp/"

data Gallery = Gallery {
      name :: String,
      parent :: Maybe String,
      readCapabilityName :: String,
      uploadCapabilityName :: String,
      administrationCapabilityName :: String
    } deriving Show

-- | Display preview picture and EXIF information
displayPreview :: String -> String -> String -> [(String, String)] -> X.Html
displayPreview caption description previewName exif =
    let doc = readMarkdown defaultParserState description
        desc = writeHtml defaultWriterOptions doc
    in (X.h1 X.<< X.stringToHtml caption) 
       X.+++ X.thediv X.<< desc X.+++ X.image X.! [X.src previewName]
       X.+++ (exifDiv exif)

-- | All interpretable EXIF data for fname                            
-- currently excludes MakerNote
exifData :: Bool -> String -> IO [(String, String)]
exifData isTemp fname = 
    let dir = case isTemp of
                True -> tempDirectory
                False -> imageDirectory
    in fromFile (dir ++ fname) >>= allTags

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
    in X.thediv X.<< tagTable pairs
   
-- | XHtml table of Exif information
tagTable :: [(String, Maybe String)] -> X.Html
tagTable tags = 
    X.table X.<< X.tbody X.<< (map tagDisplay tags)

-- | XHtml fragment to display a tag    
tagDisplay :: (String, Maybe String) -> X.Html
tagDisplay (tag, value) =
    case value of
      Nothing -> X.noHtml
      Just v -> X.tr X.<< ((X.td X.<< X.stringToHtml tag) X.+++ (X.td X.<< X.stringToHtml v))

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
divImageGallery = X.thediv X.<< (X.anchor X.! [X.href imageGalleryURL] X.<< "Image gallery")

-- | Handler for images
handleImages :: MyServerPartT Response 
handleImages = do
                 rq <- askRq
                 let paths = rqPaths rq
                 if null paths then mzero else
                     fileServeStrict [] imageDirectory

-- | Handler for imageGalleryURL
handleImageGallery :: MyServerPartT Response 
handleImageGallery = dir (tail imageGalleryURL) $ do
  rq <- askRq
  let cookies = rqCookies rq
  let sc = lookup sessionCookie cookies

  ApplicationState db sessions <- lift ask
  sess <- liftIO $ readMVar sessions
  galleries <- liftIO $ topLevelGalleries db
  authorizedGalleries <- liftIO $ filterM (isGalleryAuthorized sc sess) galleries
  authorizedGalleryHeadlines <- liftIO $ mapM (readHeadline db) authorizedGalleries
  ok $ toResponse $ X.body X.<< galleriesDiv authorizedGalleryHeadlines

-- | Gallery name, number of pictures, and if non-zero, latest picture, and upload date
data GalleryHeadline =  GalleryHeadline {gName ::String,             -- ^Name of gallery
                                         count :: Int,               -- ^Number of pictures (including all in sub-galleries)
                                         picture :: Maybe (String, CalendarTime) -- ^File name and upload time of latest picture 
                                        }

-- | Read in sufficient information to form a gallery headline display
readHeadline :: Database -> Gallery -> IO GalleryHeadline
readHeadline db gallery = do
  -- get the list of all (recursive) child gallery names
  gNames <- unfoldTreeM_BF (childGalleries db) (name gallery)
  imageNumbers <- mapM (images db) (Data.Tree.flatten gNames)
  let indices = nub (concat imageNumbers)
  recentUpload <- mostRecentImage db indices
  return $ GalleryHeadline (name gallery) (length indices) recentUpload

-- | Most recent thumbnail-image from list
mostRecentImage :: Database -> [Integer] -> IO (Maybe (String, CalendarTime))
mostRecentImage db indices =
  if null indices then return Nothing else
      do let q 
                 = do t <- DB.table IT.imageTable
                      DB.restrict (t DB.! IT.indexNumber `DB._in` map DB.constant indices)
                      DB.order [DB.desc t IT.uploadTime]
                      DB.top 1
                      DB.project (IT.thumbnail DB.<< t DB.! IT.thumbnail DB.# IT.uploadTime DB.<< t DB.! IT.uploadTime)
         rs <- DB.query db q
         return $ Just (head rs DB.! IT.thumbnail, head rs DB.! IT.uploadTime)

-- | Display headline list of galleries      
galleriesDiv :: [GalleryHeadline] -> X.Html
galleriesDiv galleries =
    X.thediv X.<< (X.h1 X.<< "Image galleries" X.+++ X.thediv X.<< map displayGallery galleries)

-- | Display one gallery headline
displayGallery :: GalleryHeadline -> X.Html
displayGallery (GalleryHeadline name count picture) =
 X.thediv X.<< name X.+++
  let toBe = case count of
               1 -> "is "
               _ -> "are "
      pictureNoun = case count of
                      1 -> " picture"
                      _ -> " pictures"
  in case count of
       0 -> X.p X.<< "There are no pictures in this gallery"
       _ -> case picture of
             Just (image, date) -> ((X.image X.! [X.src image]) X.+++
                                                               X.p X.<< ("There " ++ toBe ++ (show count) ++ pictureNoun ++ 
                                                                    " in this gallery.")
                                                               X.+++ (X.p X.<< 
                                                                      ("Last updated: " 
                                                                       ++ (calendarTimeToString date ++ " UTC"))))

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
childGalleries :: Database -> String -> IO (String, [String])
childGalleries db gName = do
  let q = do
        t <- DB.table GT.galleryTable
        DB.restrict (t DB.! GT.parentGalleryName DB..==. DB.constJust gName)
        DB.project (GT.galleryName DB.<<  t DB.! GT.galleryName)
  rs <- DB.query db q
  return (gName, map (DB.! GT.galleryName) rs)

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

