module ImageGallery (
                     divImageGallery,
                     handleImageGallery
                    ) where

import Text.XHtml.Strict
import URISpace
import ApplicationState
import Happstack.Server.SimpleHTTP

-- | Html div to invoke image gallery
divImageGallery :: Html
divImageGallery = thediv << (anchor ! [href $ imageGalleryURL] << "Image gallery")

handleImageGallery :: MyServerPartT Response 
handleImageGallery = dir (tail imageGalleryURL) $ ok $ toResponse
                     $ body << p << "NotMuch"
                     
