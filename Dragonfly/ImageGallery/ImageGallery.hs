module Dragonfly.ImageGallery.ImageGallery (
                                            Gallery (..),
                                            authorizedUploadGalleries,
                                            allGalleries,
                                            divImageGallery,
                                            handleImageGallery
                                           ) where

import Control.Applicative.State

import qualified Data.Map as Map

import qualified Database.HaskellDB as DB
import Database.HaskellDB.Database as DB
import Database.GalleryTable

import Happstack.Server.SimpleHTTP

import qualified Text.XHtml.Strict as X

import Dragonfly.ApplicationState
import qualified Dragonfly.Authorization.Authorities as Auth
import Dragonfly.URISpace (imageGalleryURL)
import qualified Dragonfly.Authorization.User as U

data Gallery = Gallery {
      name :: String,
      parent :: Maybe String,
      readCapabilityName :: String,
      uploadCapabilityName :: String,
      administrationCapabilityName :: String
    }

-- | All galleries
allGalleries :: Database -> IO [Gallery]
allGalleries db = do
  rs <- DB.query db (DB.table galleryTable)
  return $ map newGallery rs

-- | All galleries names to which the user can upload images
authorizedUploadGalleries :: U.User -> Database -> IO [String]
authorizedUploadGalleries user db = do
  rs <- DB.query db (DB.table galleryTable)
  let frs = filter (U.authorizedTo user . (DB.! uploadImageCapabilityName)) rs
  return $ map (DB.! galleryName) frs

-- | Html div to invoke image gallery
divImageGallery :: X.Html
divImageGallery = X.thediv X.<< (X.anchor X.! [X.href imageGalleryURL] X.<< "Image gallery")

-- | Handler for imageGalleryURL
handleImageGallery :: MyServerPartT Response 
handleImageGallery = dir (tail imageGalleryURL) $ do
  rq <- askRq
  let cookies = rqCookies rq
  let sc = lookup sessionCookie cookies

  ApplicationState db sessions <- lift get
  galleries <- liftIO $ topLevelGalleries db
  authorizedGalleries <- liftIO $ filterM (isGalleryAuthorized sc sessions) galleries
  ok $ toResponse $ X.body X.<< galleriesDiv authorizedGalleries

-- | Display list of galleries      
galleriesDiv :: [Gallery] -> X.Html
galleriesDiv galleries =
    X.thediv X.<< X.ulist X.<< map displayGallery galleries

-- | Display one gallery line
displayGallery :: Gallery -> X.Html
displayGallery gallery =
 X.li X.<< name gallery
 
-- | Get list of all top-level galleries from database
topLevelGalleries :: Database -> IO [Gallery]
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

