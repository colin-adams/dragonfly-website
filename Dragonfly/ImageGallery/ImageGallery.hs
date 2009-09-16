module Dragonfly.ImageGallery.ImageGallery (
                                            divImageGallery,
                                            handleImageGallery
                                           ) where

import Control.Applicative.State

import qualified Data.Map as Map

import qualified Database.HaskellDB as DB
import Database.HaskellDB.Database as DB
import Database.GalleryTable

import Happstack.Server.SimpleHTTP

import Text.XHtml.Strict

import Dragonfly.ApplicationState
import Dragonfly.URISpace

data Gallery = Gallery {
      name :: String,
      parent :: Maybe String,
      readCapabilityName :: String,
      uploadCapabilityName :: String,
      administrationCapabilityName :: String
    }

-- | Html div to invoke image gallery
divImageGallery :: Html
divImageGallery = thediv << (anchor ! [href imageGalleryURL] << "Image gallery")

-- | Handler for imageGalleryURL
handleImageGallery :: MyServerPartT Response 
handleImageGallery = do
  rq <- askRq
  let cookies = rqCookies rq
  let sc = lookup sessionCookie cookies

  ApplicationState db _ <- lift get
  galleries <- liftIO $ topLevelGalleries db
  authorizedGalleries <- liftIO $ filterM (isGalleryAuthorized sc) galleries
  dir (tail imageGalleryURL) $ ok $ toResponse $ body << p << "NotMuch"
      
-- | Get list of all top-level galleries from database
topLevelGalleries :: DB.Database -> IO [Gallery]
topLevelGalleries db = do
  let q = do
        t <- DB.table galleryTable
        DB.restrict (DB.isNull $ t DB.! parentGalleryName)
        return t
  rs <- DB.query db q
  return $ map newGallery rs

-- | Contruct a Gallery from its database record
--newGallery :: DB.Record vr -> Gallery
newGallery rec = Gallery (rec DB.! galleryName) (rec DB.! parentGalleryName) (rec DB.! readImageCapabilityName)  (rec DB.! uploadImageCapabilityName)  (rec DB.! administerGalleryCapabilityName) 

-- | Check authorization of user to view gallery
isGalleryAuthorized :: Maybe Cookie -> Gallery -> IO Bool
isGalleryAuthorized cook gallery = do
  return True

