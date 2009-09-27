-- | Form for uploading images
module Dragonfly.ImageGallery.Upload (
                                      handleImageUpload
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

import Graphics.GD
import qualified Graphics.Exif as Exif

import Happstack.Server

import Network.URL

import Text.Formlets (runFormState, FormSelection (..))
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict.Formlets as F

import qualified Database.ImageTable as IT

import Dragonfly.ApplicationState
import Dragonfly.Authorization.Auth
import Dragonfly.Authorization.User
import Dragonfly.ImageGallery.ImageGallery
import Dragonfly.URISpace (imageUploadURL)
import Dragonfly.Forms hiding (withForm)

import Debug.Trace

-- | Handler for imageUploadURL
handleImageUpload :: MyServerPartT Response 
handleImageUpload = dir (tail imageUploadURL) $ withSession uploadImagePage loginRequired

-- | XHtml page to upload an image to a gallery
uploadImagePage :: User -> MyServerPartT Response
uploadImagePage user = do
  ApplicationState db _ <- ask
  gNames <- liftIO $ authorizedUploadGalleries user db
  gs <- liftIO $ allGalleries db
  withForm (uploadImageForm gs gNames) showErrorsInline uploadImage

-- | Gathered form data
data UploadData = UploadData { 
      caption :: String,
      galleryName :: String,
      imageFile :: F.File
                             }
-- | Handle form for both GET and PUT methods
withForm :: XForm a -> (X.Html -> [String] -> MyServerPartT Response) -> (a -> MyServerPartT Response) -> MyServerPartT Response 
withForm frm handleErrors handleOk = msum
  [methodSP GET $ createForm [] frm >>= okHtml
  , withDataFn ask $ methodSP POST . handleOk' . buildEnvironment
  ]
  where
    handleOk' d = do
      let (extractor, html, _) = runFormState d frm
      v <- liftIO extractor  
      case v of
        Failure faults -> do 
          f <- createForm d frm
          handleErrors f faults
        Success s      -> handleOk s

-- | Build an environment of form answers from the inputs.
buildEnvironment :: ([(String, Input)], [(String, Cookie)]) -> F.Env
buildEnvironment (input, _) = 
    let input' = filter noSubmit input
        input'' = groupBy sameKey input' 
        input''' = map head input''
    in map toEnvElement input'''

toEnvElement :: (String, Input) -> (String, FormSelection)
toEnvElement (key, (Input cont fName ctype)) =
    case fName of
      Just f  -> (key, SingleFile $ F.File cont f (toFormContentType ctype))
      Nothing -> (key, SingleString $ LU.toString $ cont)

-- | Are left elements equal?
sameKey :: (String, Input) -> (String, Input) -> Bool
sameKey first second = fst first == fst second

-- | Drop submit button
noSubmit :: (String, Input) -> Bool
noSubmit (s, _) =
    case s of
      "submit" -> False
      _ -> True

-- | Convert from Happstack to Formlets ContentType
toFormContentType :: ContentType -> F.ContentType
toFormContentType ct = F.ContentType (ctType ct) (ctSubtype ct) (ctParameters ct)

-- | Process submitted form
uploadImage :: UploadData -> MyServerPartT Response
uploadImage udata = do
  let imageType = F.ctSubtype (F.contentType (imageFile udata))
      fname = F.fileName $ imageFile udata
      fnames = (fname, fname, Just fname) -- TODO
  case imageType of
    "jpeg" -> do
      image <- liftIO $ loadJpegByteString (pack . LB.unpack $ F.content $ imageFile udata)
      liftIO $ saveJpegFile 85 ("files/" ++ (F.fileName $ imageFile udata)) image -- TODO - prompt for quality and name
      exif <- liftIO $ Exif.fromFile ("files/" ++ (F.fileName $ imageFile udata))
      tags <- liftIO $ Exif.allTags exif
      liftIO $ mapM_ (putStrLn . show) tags
  ApplicationState db _ <- ask
  liftIO $ DB.transaction db (saveImageInfo db (caption udata) fnames)
  okHtml $ X.p << (galleryName udata ++ " uploaded with title " ++ caption udata ++ ", image file name is " ++ (F.fileName $ imageFile udata) ++ 
                                   ", content type is " ++ (show (F.contentType (imageFile udata))) ++ ", (well, not really - TODO)")

-- | Save image information to database
saveImageInfo :: Database -> String -> (String, String, Maybe String) -> IO ()
saveImageInfo db caption (thumbnailName, previewName, originalName) = do
  let nextIndex = 1 -- TODO
  DB.insert db IT.imageTable (IT.indexNumber <<- nextIndex  # IT.caption <<- caption # IT.thumbnail <<- thumbnailName # 
                                IT.preview <<- previewName # IT.original <<- originalName)


-- | Process data from form
uploadImageForm :: [Gallery] -> [String] -> XForm UploadData
uploadImageForm gs gNames = UploadData <$>  titleForm <*> (gallerySelectFormlet (galleryTree gs gNames) (Just chooseGalleryInstruction)) <*>
                            imageInputForm

-- | Gallery selection widget builder
gallerySelectFormlet :: Tree (Gallery, Bool) -> F.XHtmlFormlet IO String
gallerySelectFormlet tree defaultValue = 
    list `F.check` F.ensure valid error
  where list = F.selectRaw [X.multiple, X.size "6"] 
               ((chooseGalleryInstruction, X.p << chooseGalleryInstruction) :
                (mapMaybe gallerySelection . Data.Tree.flatten . augmentedTreeNode (-2) $ tree))
               defaultValue
        valid = (fromJust defaultValue /=)
        error = "You must select at least one gallery, but you may not select " ++ chooseGalleryInstruction

chooseGalleryInstruction :: String
chooseGalleryInstruction = "<Choose one or more>"

-- | Prompt for picture's title
titleForm :: XForm String
titleForm = form `F.check` F.ensure valid error
    where form = "Title" `label` F.input Nothing
          valid = not . null
          error = "Image title must be supplied"

-- | Prompt for image file
imageInputForm :: XForm F.File
imageInputForm = form `F.check` F.ensure validImageFile error
    where form = F.plug (\xhtml -> X.p << (X.label << "Image file:") +++ xhtml) F.file
          error = "Image file must be selected"

validImageFile :: F.File -> Bool
validImageFile (F.File cont fName ct) = not . null $ fName 

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
