-- | Form for uploading images
module Dragonfly.ImageGallery.Upload where

import Control.Applicative.State

import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Tree

import Happstack.Server

import Network.URL

import Text.XHtml.Strict
import Text.Formlets.MassInput
import Text.XHtml.Strict.Formlets

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
  ApplicationState db _ <- lift get
  gNames <- liftIO $ authorizedUploadGalleries user db
  gs <- liftIO $ allGalleries db
  withForm imageUploadURL (gallerySelectFormlet (galleryTree gs gNames) Nothing) showErrorsInline uploadImage

uploadImage :: String -> MyServerPartT Response
uploadImage name = okHtml $ p << (name ++ " uploaded")

gallerySelectFormlet :: Tree (Gallery, Bool) -> XHtmlFormlet IO String
gallerySelectFormlet =
    selectRaw [multiple, size "6"] . mapMaybe gallerySelection . toList

-- | Selection-list widget for authorized galleries
gallerySelection :: (Gallery, Bool) -> Maybe (String, Html)
gallerySelection (gallery, selectable) =
    let nm = Dragonfly.ImageGallery.ImageGallery.name gallery 
    in if selectable then
           Just (nm, p << nm)
       else Nothing

-- | Galleries arranged as a tree, with upload authorization status
galleryTree :: [Gallery] -> [String] -> Tree (Gallery, Bool)
galleryTree gs authNames = Node {rootLabel = (rootGallery, False), subForest = childTrees Nothing gs authNames}

-- | Placeholder for top-level-galleries
rootGallery :: Gallery
rootGallery = Gallery "" Nothing "" "" ""

childTrees :: Maybe String -> [Gallery] -> [String] -> Forest (Gallery, Bool)
childTrees par galleries authNames =
    let children = filter (\g -> par == parent g && (Dragonfly.ImageGallery.ImageGallery.name g `elem` authNames)) galleries
    in map (\child -> Node {rootLabel = (child, False), subForest = childTrees (parent child) galleries authNames}) children

loginRequired :: MyServerPartT Response
loginRequired = seeOther (encString False ok_url "/?_message=Login required") (toResponse "")
