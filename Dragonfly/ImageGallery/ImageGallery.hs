module Dragonfly.ImageGallery.ImageGallery (
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
import qualified Data.Map as Map
import Data.Ratio

import Graphics.Exif
import Graphics.GD

import Happstack.Server.SimpleHTTP
import Happstack.Server.HTTP.FileServe

import Numeric

import System.Environment

import qualified Text.XHtml.Strict.Formlets as F
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((<<), (+++)) 
import Text.Pandoc.Shared
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Dragonfly.ApplicationState
import Dragonfly.Forms
import Dragonfly.ImageGallery.Database
import Dragonfly.ImageGallery.Exif
import Dragonfly.ImageGallery.GalleryPage
import Dragonfly.URISpace

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
      pageParam = lookup pageParameter params
  case previewParam of
       Just p -> displayPreviewPicture $ LU.toString $ inputValue p
       Nothing -> displayGalleryTree galleryParam pageParam sc

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




