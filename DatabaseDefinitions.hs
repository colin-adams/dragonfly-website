-- | Program to generate website database and associated Haskell module
module Main where

import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec.DBSpecToDBDirect
import Database.HaskellDB.DBSpec.DBSpecToDatabase
import Database.HaskellDB.DBSpec.DBInfo
import Database.HaskellDB.DBSpec.PPHelpers
import Database.HaskellDB.HDBC.PostgreSQL

-- | Database used throughout the website
database :: DBInfo
database = DBInfo {dbname = "website", opts = databaseOptions, tbls = databaseTables}

databaseOptions :: DBOptions
databaseOptions = DBOptions {useBString = False, makeIdent = mkIdentPreserving}

databaseTables :: [TInfo]
databaseTables = [userTable, authTable, userAuthTable, capabilitiesTable, authCapabilitiesTable, galleryTable, imageTable, galleryImageTable]

-- | Definition of users.
-- | Enabled users also have a list of authorization groups in user_auth_table
userTable :: TInfo
userTable = TInfo {tname = "userTable", cols = [userColumn, passwordColumn, enabledColumn]}

-- | All defined authorization groups
authTable :: TInfo
authTable = TInfo {tname = "authTable", cols = [authColumn]}

-- | Authorization groups associated with each user
userAuthTable :: TInfo
userAuthTable = TInfo {tname = "userAuthTable", cols = [userColumn, authColumn]}

-- | All defined authorization capabilities
capabilitiesTable :: TInfo
capabilitiesTable = TInfo {tname = "capabilitiesTable", cols = [capabilityColumn]}

-- | Capabilities associated with each authority group
authCapabilitiesTable :: TInfo
authCapabilitiesTable = TInfo {tname = "authCapabilitiesTable", cols = [authColumn, capabilityColumn]}

-- | Definitions of image galleries
galleryTable :: TInfo
galleryTable = TInfo {tname = "galleryTable", cols = [galleryNameColumn, parentGalleryNameColumn, readImageCapabilityNameColumn,
                                                                       uploadImageCapabilityNameColumn, administerGalleryCapabilityNameColumn]}

-- | Relative URIs for various sized images
imageTable :: TInfo
imageTable = TInfo {tname = "imageTable", cols = [indexColumn, captionColumn, thumbnailColumn, previewColumn, originalColumn, uploadTimeColumn]}

-- | Images in each gallery
galleryImageTable :: TInfo
galleryImageTable = TInfo {tname = "galleryImageTable", cols = [galleryNameColumn, indexColumn]}

captionColumn :: CInfo
captionColumn = CInfo {cname = "caption", descr = (StringT, False)}

galleryNameColumn :: CInfo
galleryNameColumn = CInfo {cname = "galleryName", descr = (StringT, False)}

parentGalleryNameColumn :: CInfo
parentGalleryNameColumn = CInfo {cname = "parentGalleryName", descr = (StringT, True)}

readImageCapabilityNameColumn :: CInfo
readImageCapabilityNameColumn = CInfo {cname = "readImageCapabilityName", descr = (StringT, False)}

uploadImageCapabilityNameColumn :: CInfo
uploadImageCapabilityNameColumn = CInfo {cname = "uploadImageCapabilityName", descr = (StringT, False)}

administerGalleryCapabilityNameColumn :: CInfo
administerGalleryCapabilityNameColumn = CInfo {cname = "administerGalleryCapabilityName", descr = (StringT, False)}

userColumn :: CInfo
userColumn = CInfo {cname = "userName", descr = (StringT, False)}

authColumn :: CInfo
authColumn = CInfo {cname = "authName", descr = (StringT, False)}

passwordColumn :: CInfo
passwordColumn = CInfo {cname = "password", descr = (StringT, False)}

enabledColumn :: CInfo
enabledColumn = CInfo {cname = "enabled", descr = (BoolT, False)}

capabilityColumn :: CInfo
capabilityColumn = CInfo {cname = "capability", descr = (StringT, False)}

indexColumn :: CInfo
indexColumn = CInfo {cname = "indexNumber", descr = (IntegerT, False)}

thumbnailColumn :: CInfo
thumbnailColumn = CInfo {cname = "thumbnail", descr = (StringT, False)}

previewColumn :: CInfo
previewColumn = CInfo {cname = "preview", descr = (StringT, False)}

originalColumn :: CInfo
originalColumn = CInfo {cname = "original", descr = (StringT, False)}

uploadTimeColumn :: CInfo
uploadTimeColumn = CInfo {cname = "uploadTime", descr = (CalendarTimeT, False)}

main :: IO ()
main = do
  -- Generate the Haskell module for use in the application
  dbInfoToModuleFiles "" "Database" database
                      
  -- Generate the database
  postgresqlConnect [] $ \db -> dbSpecToDatabase db database
