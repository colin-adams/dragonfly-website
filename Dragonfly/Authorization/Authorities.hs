module Dragonfly.Authorization.Authorities where

-- | Name of administrator group
administratorAuthority :: String
administratorAuthority = "admin"

-- | Capabilities known to always exist
knownCapabilities :: [String]
knownCapabilities = [readGalleryCapabilityName, uploadImageCapabilityName, administerGalleryCapabilityName]

-- | Authority to read galleries with no additional priviledges
readGalleryCapabilityName :: String
readGalleryCapabilityName = "readGalleryCapability"

-- | Authority to upload images to galleries with no additional priviledges
uploadImageCapabilityName :: String
uploadImageCapabilityName = "uploadImageCapability"

-- | Authority to adminster galleries with no additional priviledges
administerGalleryCapabilityName :: String
administerGalleryCapabilityName = "administerGalleryCapability"
