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

import Database.HaskellDB (transaction)

import Directory (doesFileExist)

import Happstack.Server

import Network.URL

import System.FilePath.Posix (splitExtension)
import System.Directory (copyFile, removeFile)
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
import Dragonfly.ImageGallery.Database
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
  withPreviewForm X.noHtml [] (uploadImageForm gs gNames) (True, False)
                  showErrorsInline (previewImageUpload user)

-- | Process form for GET and PUT methods with preview button, and/or submit button
withPreviewForm :: X.HTML b => b -- ^ Optional HTML fragment to prepend to form
                -> F.Env         -- ^ Environment of form values
                -> XForm a       -- ^ Form to display
                -> (Bool, Bool)  -- ^ Do we add (preview, submit) buttons?
                -> (X.Html -> [String] -> MyServerPartT Response) -- ^ Error handler
                -> (a -> F.Env -> Bool -> XForm a -> MyServerPartT Response) -- ^ Success handler
                -> MyServerPartT Response 
withPreviewForm prologue env frm (withPreview, withSubmit) handleErrors handleOk =
  msum [methodSP GET $ formMaker env frm >>= okHtml
       , withDataFn ask $ methodSP POST . handleOk' . buildEnvironment
       ]
  where
    formMaker = case (withPreview, withSubmit) of
                  (True, False) -> createPreview prologue
                  (True, True) -> createPreviewSubmit prologue
                  (False, True) -> createForm prologue
    handleOk' (d, sub) = do
      let (extractor, html, _) = runFormState d frm
      v <- liftIO extractor  
      case v of
        Failure faults -> do 
               f <- formMaker d frm
               handleErrors f faults
        Success s -> handleOk s d sub frm

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
      Just f -> (key, Right $ F.File cont f (toFormContentType ctype))
      Nothing -> (key, Left $ LU.toString cont)

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
previewImageUpload :: User -> UploadData -> F.Env -> Bool -> XForm UploadData -> MyServerPartT Response
previewImageUpload user udata env sub frm = do
  iDir <- liftIO imageDirectory
  let f = imageFile udata
      dir = if sub then iDir else tempDirectory
  let contents = F.content f
      (usePrevious, fName, imageType) = if LB.length contents == 0
       then (True, previousFileName udata, previousCT udata)
       else (False, F.fileName $ imageFile udata, F.ctSubtype $ F.contentType $ imageFile udata)
  fnames@(thumbnailName, previewName, fname) <- liftIO $ imageNames fName (not sub)
  if usePrevious
    then
       if null fName
        then displayNoFileError env frm
        else if validImageType imageType
           then if sub 
               then do
                 liftIO $ makeTempImagePermanent fnames
                 saveToDatabase fnames imageType
                 exif <- liftIO $ exifData False fname
                 okHtml $ displayPreview (caption udata) (description udata) previewName fname exif
               else do
                 saveToDatabase fnames imageType
                 displayPreviewPage udata dir fnames imageType env frm
            else displayInvalidImage f env frm
    else
       if validImageFile f
         then do
           liftIO $ LB.writeFile (dir ++ fname) contents
           if sub 
             then do
               liftIO $ saveFiles dir fnames imageType
               saveToDatabase fnames imageType
               exif <- liftIO $ exifData False fname
               okHtml $ displayPreview (caption udata) (description udata) previewName fname exif
             else displayPreviewPage udata dir fnames imageType env frm
         else displayInvalidImage f env frm
 where saveToDatabase fnames imageType = do
         ApplicationState db _ <- ask
         liftIO $ transaction db (saveImageInfo user db (caption udata) (description udata) 
                                  (galleryNames udata) imageType fnames)

-- | Re-display the form with a request to select a file
displayNoFileError :: F.Env -- ^ Environment of previous form values
                   -> XForm UploadData   -- ^ Form to display
                   -> MyServerPartT Response 
displayNoFileError env frm =
    createPreview (X.p X.<< "You must select a file") env frm >>= okHtml

-- | Move temporary files to permanent directory
makeTempImagePermanent :: (String, String, String) -> IO ()
makeTempImagePermanent fnames@(thumbnailName, previewName, fname) = do
  iDir <- imageDirectory
  copyFile (tempDirectory ++ thumbnailName) (iDir ++ thumbnailName)
  removeFile (tempDirectory ++ thumbnailName)
  copyFile (tempDirectory ++ previewName) (iDir ++ previewName)
  removeFile (tempDirectory ++ previewName)
  copyFile (tempDirectory ++ fname) (iDir ++ fname)
  removeFile (tempDirectory ++ fname)

displayInvalidImage :: F.File -- ^ File that was uploaded
                    -> F.Env -- ^ Environment of previous form values
                    -> XForm UploadData   -- ^ Form to display
                    -> MyServerPartT Response 
displayInvalidImage file env frm =
    createPreview (X.p X.<< (F.fileName file ++ " is not a supported image type")) env frm >>= okHtml

-- | Names of thumbnail, preview and original images respectively
imageNames :: String -> Bool -> IO (String, String, String)
imageNames file isTemp = do
  original <- toOriginal isTemp file
  let thumbnailName = toThumbnail original
      previewName = toPreview original
  return (thumbnailName, previewName, original)

-- | Process data from form
uploadImageForm :: [Gallery] -> [String] -> XForm UploadData
uploadImageForm gs gNames = UploadData <$>  titleForm <*> 
                            gallerySelectFormlet (galleryTree gs gNames) (Just [chooseSelection]) <*>
                            imageInputForm <*> descriptionForm <*> F.hidden Nothing <*> F.hidden Nothing

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
descriptionForm = F.check form stripCRs 
    where form = "Description" `label` F.textarea (Just 8) (Just 80) (Just "")
          stripCRs = Success . filter ( not . (`elem` "\r"))

-- | Prompt for image file
imageInputForm :: XForm F.File
imageInputForm = form
    where form = F.plug (\xhtml -> X.p << (X.label << "Image file:") +++ xhtml) F.file

-- | Is the image type supported by Graphics.GD?
validImageType :: String -> Bool
validImageType it =
    case it of
      "jpeg" -> True
      "png" -> True
      "gif" -> True
      _ -> False

-- | Is the file an image type supported by Graphics.GD?
validImageFile :: F.File -> Bool
validImageFile (F.File cont fName ct) =
    case F.ctType ct of
      "image" -> validImageType $ F.ctSubtype ct
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
    let nm = Dragonfly.ImageGallery.Database.name gallery 
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
    in map (\child -> Node {rootLabel = (child,  Dragonfly.ImageGallery.Database.name child `elem` authNames), subForest = childTrees (Just $ Dragonfly.ImageGallery.Database.name child) galleries authNames}) children

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
  iDir <- imageDirectory
  let dir = if isTemp
            then tempDirectory
            else iDir
  exists <- doesFileExist (dir ++ fname)
  if exists then
      do
        let (base, ext) = splitExtension fname
        toOriginal isTemp (base ++ "0" ++ ext)
      else return fname
