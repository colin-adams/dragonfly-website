-- | Form for uploading images
module Dragonfly.ImageGallery.Upload where

import Control.Applicative
import Control.Applicative.Error
import Control.Monad.Reader

import qualified Data.ByteString.Lazy.UTF8 as LU
import qualified Data.Foldable as Fo (foldr)
import Data.Maybe (mapMaybe, fromJust)
import Data.Tree

import Happstack.Server

import Network.URL

import Text.Formlets (runFormState)
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict.Formlets as F

import Dragonfly.ApplicationState
import Dragonfly.Authorization.Auth
import Dragonfly.Authorization.User
import Dragonfly.ImageGallery.ImageGallery
import Dragonfly.URISpace (imageUploadURL)
import Dragonfly.Forms

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
  processForm (uploadImageForm gs gNames) showErrorsInline uploadImage

-- | Gathered form data
data UploadData = UploadData { 
      caption :: String,
      galleryName :: String,
      imageFile :: F.File
                             }
-- | Process form for GET and PUT methods
processForm :: XForm a -> (X.Html -> [String] -> MyServerPartT Response) -> (a -> MyServerPartT Response) -> MyServerPartT Response 
processForm frm handleErrors handleOk = msum
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

-- | Build an environment of form answers from the inputs
buildEnvironment :: ([(String, Input)], [(String, Cookie)]) -> F.Env
buildEnvironment (input, _) = 
    case input of
      x:y:z:_submit:[] -> [(fst x, Left $  LU.toString $ inputValue $ snd x),
                           (fst y, Left $ LU.toString $ inputValue $ snd y),
                           (fst z, f $ snd z)]
          where f (Input cont fName ctype) = Right $ F.File cont (fromJust fName) (toFormContentType ctype)
      _ -> trace (show input) []

toFormContentType :: ContentType -> F.ContentType
toFormContentType ct = F.ContentType (ctType ct) (ctSubtype ct) (ctParameters ct)

-- | Process submitted form
uploadImage :: UploadData -> MyServerPartT Response
uploadImage udata = okHtml $ X.p << (galleryName udata ++ " uploaded with title " ++ caption udata ++ ", image file name is " ++ (F.fileName $ imageFile udata) ++ ", (well, not really - TODO)")


-- | Process data from form
uploadImageForm :: [Gallery] -> [String] -> XForm UploadData
uploadImageForm gs gNames = UploadData <$>  titleForm <*> (gallerySelectFormlet (galleryTree gs gNames) Nothing) <*>
                            imageInputForm

-- | Gallery selection widget builder
gallerySelectFormlet :: Tree (Gallery, Bool) -> F.XHtmlFormlet IO String
gallerySelectFormlet = F.selectRaw [X.multiple, X.size "6"] . mapMaybe gallerySelection . Data.Tree.flatten . augmentedTreeNode (-2)

-- | Prompt for picture's title
titleForm :: XForm String
titleForm = "Title" `label` F.input Nothing

-- | Prompt for image file
imageInputForm :: XForm F.File
imageInputForm = F.plug (\xhtml -> X.p << (X.label << "Image file:") +++ xhtml) F.file

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
