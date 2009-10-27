-- | URIs and parameter names
module Dragonfly.URISpace where

signOutURL :: String
signOutURL = "/_signout"

loginURL :: String
loginURL = "/_login"

registerURL :: String
registerURL = "/_register"

imageGalleryURL :: String
imageGalleryURL = "/image_gallery"

imageUploadURL :: String
imageUploadURL = "/upload_image"

-- | Name of URI parameter giving gallery name
galleryParameter :: String
galleryParameter = "gallery"

-- | Name of URI parameter giving preview picture name
previewParameter :: String
previewParameter = "preview"

-- | Name of URI parameter giving page number for many pictures
pageParameter :: String
pageParameter = "page"

