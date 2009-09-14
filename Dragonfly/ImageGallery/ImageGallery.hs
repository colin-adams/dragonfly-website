module Dragonfly.ImageGallery.ImageGallery (
                                            divImageGallery,
                                            handleImageGallery
                                           ) where


import qualified Data.Map as Map
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

handleImageGallery :: MyServerPartT Response 
handleImageGallery = dir (tail imageGalleryURL) $ ok $ toResponse
                     $ body << p << "NotMuch"
                     
