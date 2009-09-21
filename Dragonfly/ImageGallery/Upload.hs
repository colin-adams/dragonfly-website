-- | Form for uploading images
module Dragonfly.ImageGallery.Upload where

import Control.Applicative
import Control.Monad.Reader

import qualified Data.Foldable as Fo (foldr)
import Data.Maybe (mapMaybe)
import Data.Tree

import Happstack.Server

import Network.URL

import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict.Formlets as F

import Dragonfly.ApplicationState
import Dragonfly.Authorization.Auth
import Dragonfly.Authorization.User
import Dragonfly.ImageGallery.ImageGallery
import Dragonfly.URISpace (imageUploadURL)
import Dragonfly.Forms

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

-- | Process submitted form
uploadImage :: UploadData -> MyServerPartT Response
uploadImage udata = okHtml $ X.p << (galleryName udata ++ " uploaded with title " ++ caption udata ++ ", image file name is " ++ (F.fileName $ imageFile udata) ++ ", (well, not really - TODO)")


-- | Combined widgets for form
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
