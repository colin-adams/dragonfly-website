-- | Form for uploading images
module Dragonfly.ImageGallery.Upload (
                                      handleImageUpload,
                                      divImageUpload
                                     ) where

import Control.Applicative
import Control.Applicative.Error
import Control.Monad.Reader

import Data.ByteString (pack)
import qualified Data.ByteString.Lazy.UTF8 as LU
import qualified Data.ByteString.Lazy as LB
import Data.List (groupBy)
import qualified Data.Foldable as Fo (foldr)
import Data.Maybe (mapMaybe, fromJust)
import Data.Tree

import qualified Database.HaskellDB as DB
import Database.HaskellDB  (Database, (<<-), (#))

import Directory (doesFileExist)

import Graphics.GD

import Happstack.Server

import Network.URL

import System.FilePath.Posix (splitExtension)
import System.Time

import Text.Formlets (runFormState)
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict.Formlets as F

import qualified Database.ImageTable as IT
import qualified Database.GalleryImageTable as GIT

import Dragonfly.ApplicationState
import Dragonfly.Authorization.Auth
import Dragonfly.Authorization.User
import Dragonfly.ImageGallery.ImageGallery
import Dragonfly.URISpace (imageUploadURL)
import Dragonfly.Forms

-- | Handler for imageploadURL
handleImageUpload :: MyServerPartT Response 
handleImageUpload = dir (tail imageUploadURL) $ withSession uploadImagePage loginRequired

-- | Html div to invoke uploading an image
divImageUpload :: X.Html
divImageUpload = X.thediv X.<< (X.anchor X.! [X.href imageUploadURL] X.<< "Upload an image")

-- | XHtml page to upload an image to a gallery
uploadImagePage :: User -> MyServerPartT Response
uploadImagePage user = do
  ApplicationState db _ <- ask
  gNames <- liftIO $ authorizedUploadGalleries user db
  gs <- liftIO $ allGalleries db
  withPreviewForm (uploadImageForm gs gNames) (True, False) showErrorsInline previewImageUpload

-- | Gathered form data
data UploadData = UploadData { 
      caption :: String,
      galleryNames :: [String],
      imageFile :: F.File,
      description :: String
                             }
-- | Process form for GET and PUT methods with preview button, and/or submit button
withPreviewForm :: XForm a -> (Bool, Bool) -> (X.Html -> [String] -> MyServerPartT Response) -> (a -> MyServerPartT Response) -> MyServerPartT Response 
withPreviewForm frm (withPreview, withSubmit) handleErrors handleOk = msum
  [methodSP GET $ c [] frm >>= okHtml
  , withDataFn ask $ methodSP POST . handleOk' . buildEnvironment
  ]
  where
    c = case (withPreview, withSubmit) of
          (True, False) -> createPreview
          (True, True)  -> createPreviewSubmit
          (False, True) -> createForm
    handleOk' (d, sub) = do
      let (extractor, html, _) = runFormState d frm
      v <- liftIO extractor  
      case v of
        Failure faults -> do 
          f <- createForm d frm
          handleErrors f faults
        Success s      -> handleOk s

-- | Build an environment of form answers from the inputs.
-- | Also supply an indication if Submit button was pressed.
buildEnvironment :: ([(String, Input)], [(String, Cookie)]) -> (F.Env, Bool)
buildEnvironment (input, _) = 
    let input' = filter noSubmit input
        wasSubmit = elem "submit" $ map fst input
    in (map toEnvElement input', wasSubmit)

toEnvElement :: (String, Input) -> (String, Either String F.File)
toEnvElement (key, (Input cont fName ctype)) =
    case fName of
      Just f -> (key, Right $ F.File cont (f) (toFormContentType ctype))
      Nothing -> (key, Left $ LU.toString $ cont)

-- | Drop submit and/or preview buttons
noSubmit :: (String, Input) -> Bool
noSubmit (s, _) =
    case s of
      "submit" -> False
      "preview" -> False
      _ -> True

-- | Convert from Happstack to Formlets ContentType
toFormContentType :: ContentType -> F.ContentType
toFormContentType ct = F.ContentType (ctType ct) (ctSubtype ct) (ctParameters ct)

-- | Process submitted form by showing preview page
previewImageUpload :: UploadData -> MyServerPartT Response
previewImageUpload udata = do
  let imageType = F.ctSubtype (F.contentType (imageFile udata))
  fname <- liftIO $ toOriginal True $ F.fileName $ imageFile udata
  let thumbnailName = toThumbnail fname
      previewName = toPreview fname
      fnames = (thumbnailName, previewName, fname)
  liftIO $ LB.writeFile (tempDirectory ++ fname) (F.content $ imageFile udata)
  case imageType of
    "jpeg" -> do
      image <- liftIO $ loadJpegFile (tempDirectory ++ fname)
      -- TODO - constants for sizes and quality 
      previewImage <- liftIO $ resizeImage 640 400 image
      liftIO $ saveJpegFile 85 (tempDirectory ++ previewName) previewImage
      thumbnailImage <- liftIO $ resizeImage 120 80 image
      liftIO $ saveJpegFile 70 (tempDirectory ++ thumbnailName) thumbnailImage
  ApplicationState db _ <- ask
  liftIO $ DB.transaction db (saveImageInfo db (caption udata) (description udata) 
                                                (galleryNames udata) imageType fnames)
  exif <- liftIO $ exifData True fname
  -- TODO: change to displayPreviewForm
  okHtml $ displayPreview (caption udata) ("temp/" ++ previewName) exif

-- | Save image information to database
saveImageInfo :: Database -> String -> String -> [String] -> String -> (String, String, String) -> IO ()
saveImageInfo db caption description galleries imageType (thumbnailName, previewName, originalName) = do
  let q = do
        t <- DB.table IT.imageTable
        DB.order [DB.desc t IT.indexNumber]
        DB.project (IT.indexNumber DB.<< t DB.! IT.indexNumber)
  rs <- DB.query db q
  let nextIndex = case null rs of
                    True -> 1
                    False -> 1 + (head rs DB.! IT.indexNumber)
  ct <- getClockTime 
  let utc = toUTCTime ct
  DB.insert db IT.imageTable (IT.indexNumber <<- nextIndex  # IT.caption <<- caption # 
                                IT.body <<- description # IT.thumbnail <<- thumbnailName # 
                                IT.preview <<- previewName # IT.original <<- originalName # 
                                IT.uploadTime <<- utc # IT.imageType <<- imageType)
  mapM_ (\name -> DB.insert db GIT.galleryImageTable (GIT.galleryName <<- name # GIT.indexNumber <<- nextIndex)) galleries

-- | Process data from form
uploadImageForm :: [Gallery] -> [String] -> XForm UploadData
uploadImageForm gs gNames = UploadData <$>  titleForm <*> 
                            (gallerySelectFormlet (galleryTree gs gNames) (Just [chooseSelection]))
                            <*> imageInputForm <*> descriptionForm

-- | Gallery selection widget builder
gallerySelectFormlet :: Tree (Gallery, Bool) -> F.XHtmlFormlet IO [String]
gallerySelectFormlet tree defaultValue = 
    list `F.check` F.ensure valid error
  where list = F.selectMultipleRaw 6 [] 
               ((chooseSelection, X.p << chooseSelection) :
                (mapMaybe gallerySelection . Data.Tree.flatten . augmentedTreeNode (-2) $ tree))
               defaultValue
        valid = not . (chooseSelection `elem`)
        error = "You must select at least one gallery, and you may not include " ++ chooseSelection

chooseSelection :: String
chooseSelection = "<choose one or more galleries>"

-- | Prompt for picture's title
titleForm :: XForm String
titleForm = form `F.check` F.ensure valid error
    where form = "Title" `label` F.input Nothing
          valid = not . null
          error = "Image title must be supplied"

-- | Description of picture (will be treated as written in Pandoc markdown)
descriptionForm :: XForm String
descriptionForm = "Description" `label` F.textarea (Just 8) (Just 80) (Just "")

-- | Prompt for image file
imageInputForm :: XForm F.File
imageInputForm = form `F.check` F.ensure validImageFile error
    where form = F.plug (\xhtml -> X.p << (X.label << "Image file:") +++ xhtml) F.file
          error = "A JPEG/GIF/PNG file must be selected"

-- | Is the file an image type supported by Graphics.GD?
validImageFile :: F.File -> Bool
validImageFile (F.File cont fName ct) = 
    case F.ctType ct of
      "image" -> case F.ctSubtype ct of
                  "jpeg" -> True
                  "png" -> True
                  "gif" -> True
                  _ -> False
      _ -> False

label :: String -> XForm String -> XForm String
label l = F.plug (\xhtml -> X.p << (X.label << (l ++ ": ") +++ xhtml))

-- | Add nesting depth
augmentedTreeNode :: Int -> Tree (Gallery, Bool) -> Tree (Gallery, Bool, Int)
augmentedTreeNode depth (Node (g, selectable) gs) = 
    Node (g, selectable, depth + 1) (map (augmentedTreeNode (depth + 1)) gs)

-- | Selection-list widget for authorized gallery
gallerySelection :: (Gallery, Bool, Int) -> Maybe (String, X.Html)
gallerySelection (gallery, selectable, depth) =
    let nm = Dragonfly.ImageGallery.ImageGallery.name gallery 
        hyphens = replicate depth '-'
    in if selectable then
           Just (nm, X.p << (hyphens ++ nm))
       else Nothing

-- | Galleries arranged as a tree, with upload authorization status
galleryTree :: [Gallery] -> [String] -> Tree (Gallery, Bool)
galleryTree gs authNames = Node {rootLabel = (rootGallery, False), subForest = childTrees (Just "") gs authNames}

-- | Placeholder for top-level-galleries
rootGallery :: Gallery
rootGallery = Gallery "" Nothing "" "" ""

-- | Child galleries for a gallery
childTrees :: Maybe String -> [Gallery] -> [String] -> Forest (Gallery, Bool)
childTrees par galleries authNames =
    let children = filter (childGallery par) galleries
    in map (\child -> Node {rootLabel = (child,  Dragonfly.ImageGallery.ImageGallery.name child `elem` authNames), subForest = childTrees (Just $ Dragonfly.ImageGallery.ImageGallery.name child) galleries authNames}) children

-- | Is gallery a child of par?
childGallery :: Maybe String ->  Gallery -> Bool
childGallery par g =
    case par of 
      Nothing -> False
      Just "" -> case parent g of
                   Nothing -> True
                   Just _ -> False
      Just p -> case parent g of
                  Nothing -> False
                  Just q -> p == q

loginRequired :: MyServerPartT Response
loginRequired = seeOther (encString False ok_url "/?_message=Login required") (toResponse "")

-- | Convert (possibly modified) original file name to a thumbnail name
toThumbnail :: String -> String
toThumbnail original =
    let (base, ext) = splitExtension original
    in base ++ "_thumbnail" ++ ext

-- | Convert (possibly modified) original file name to a preview name
toPreview :: String -> String
toPreview original =
    let (base, ext) = splitExtension original
    in base ++ "_preview" ++ ext

-- | Convert original file name so it doesn't clash with an existing one
toOriginal :: Bool -> String -> IO String
toOriginal isTemp fname = do
  let dir = case isTemp of
              True -> tempDirectory
              False -> imageDirectory
  exists <- doesFileExist (dir ++ fname)
  if exists then
      do
        let (base, ext) = splitExtension fname
        toOriginal isTemp (base ++ "0" ++ ext)
      else return fname
