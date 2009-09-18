-- | Form for uploading images
module Dragonfly.ImageGallery.Upload where

import Control.Applicative.State

import Data.Tree

import Happstack.Server

import Text.XHtml.Strict
import Text.Formlets.MassInput
import Text.XHtml.Strict.Formlets

import Dragonfly.ApplicationState
import Dragonfly.Authorization.Auth
import Dragonfly.Authorization.User
import Dragonfly.ImageGallery.ImageGallery
import Dragonfly.URISpace (imageUploadURL)

-- | Handler for imageUploadURL
handleImageUpload :: MyServerPartT Response 
handleImageUpload = do
  rq <- askRq
  let cookies = rqCookies rq
  let sc = lookup sessionCookie cookies

  ApplicationState db sessions <- lift get
  dir (tail imageUploadURL) $
      withSession uploadImagePage (seeOther "/?_message=Login required" (toResponse ""))

-- | XHtml page to upload an image to a gallery
uploadImagePage :: User -> MyServerPartT Response
uploadImagePage user = do
  ApplicationState db _ <- lift get
  gNames <- liftIO $ authorizedUploadGalleries user db
  gs <- liftIO $ allGalleries db
  -- gsf <- liftIO $ gallerySelectFormlet (galleryTree gs gNames)
  ok $ toResponse $ body << "" --gsf

gallerySelectFormlet :: Tree (Gallery, Bool) -> XHtmlFormlet IO [Char]
gallerySelectFormlet galleries =
    selectRaw [] [("fred", p << "Freddy")]

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
