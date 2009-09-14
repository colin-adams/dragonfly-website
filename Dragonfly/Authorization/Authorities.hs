module Dragonfly.Authorization.Authorities where

-- | Name of administrator group
administratorAuthority :: String
administratorAuthority = "admin"

-- | All capabilities in system
allCapabilities :: [String]
allCapabilities = [uploadImageCapabilityName, administerGalleryCapabilityName]

-- | Authority to upload images to galleries with no additional priviledges
uploadImageCapabilityName :: String
uploadImageCapabilityName = "uploadImageCapability"

-- | Authority to adminster galleries with no additional priviledges
administerGalleryCapabilityName :: String
administerGalleryCapabilityName = "administerGalleryCapability"
