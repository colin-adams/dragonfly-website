module Dragonfly.ImageGallery.ImageGallery (
                     divImageGallery,
                     handleImageGallery
                    ) where

import Happstack.Server.SimpleHTTP

import Text.XHtml.Strict

import Dragonfly.ApplicationState
import Dragonfly.URISpace

-- | Html div to invoke image gallery
divImageGallery :: Html
divImageGallery = thediv << (anchor ! [href imageGalleryURL] << "Image gallery")

handleImageGallery :: MyServerPartT Response 
handleImageGallery = dir (tail imageGalleryURL) $ ok $ toResponse
                     $ body << p << "NotMuch"
                     
