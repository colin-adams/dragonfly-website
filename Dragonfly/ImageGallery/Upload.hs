-- | Form for uploading images
module Dragonfly.ImageGallery.Upload where

import Control.Applicative.State

import Happstack.Server

import qualified Text.XHtml.Strict as X

import Dragonfly.ApplicationState
import Dragonfly.Authorization.Auth
import Dragonfly.URISpace (imageUploadURL)

-- | Handler for imageUploadURL
handleImageUpload :: MyServerPartT Response 
handleImageUpload = do
  rq <- askRq
  let cookies = rqCookies rq
  let sc = lookup sessionCookie cookies

  ApplicationState db sessions <- lift get
  dir (tail imageUploadURL) $
      withSession (\user -> ok $ toResponse $ X.body X.<< "Not much") (seeOther "/?_message=Login required" (toResponse ""))
