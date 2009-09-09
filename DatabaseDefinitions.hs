-- | Program to generate website database and associated Haskell module
module Main where

import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec.DBSpecToDBDirect
import Database.HaskellDB.DBSpec.DBSpecToDatabase
import Database.HaskellDB.DBSpec.DBInfo
import Database.HaskellDB.DBSpec.PPHelpers
import Database.HaskellDB.HDBC.SQLite3

-- | Database used throughout the website
database :: DBInfo
database = DBInfo {dbname = "website", opts = database_options, tbls = database_tables}

database_options :: DBOptions
database_options = DBOptions {useBString = False, makeIdent = mkIdentPreserving}

database_tables :: [TInfo]
database_tables = [user_table, auth_table, user_auth_table, capabilities_table, auth_capabilities_table]

-- | Definition of users.
-- | Enabled users also have a list of authorization groups in user_auth_table
user_table :: TInfo
user_table = TInfo {tname = "user_table", cols = [user_column, password_column, enabled_column]}

-- | All defined authorization groups
auth_table :: TInfo
auth_table = TInfo {tname = "auth_table", cols = [auth_column]}

-- | Authorization groups associated with each user
user_auth_table :: TInfo
user_auth_table = TInfo {tname = "user_auth_table", cols = [user_column, auth_column]}

-- | All defined authorization capabilities
capabilities_table :: TInfo
capabilities_table = TInfo {tname = "capabilities_table", cols = [capability_column]}

-- | Capabilities associated with each authority group
auth_capabilities_table :: TInfo
auth_capabilities_table = TInfo {tname = "auth_capabilities_table", cols = [auth_column, capability_column]}

user_column :: CInfo
user_column = CInfo {cname = "user_name", descr = (StringT, False)}

auth_column :: CInfo
auth_column = CInfo {cname = "auth_name", descr = (StringT, False)}

password_column :: CInfo
password_column = CInfo {cname = "password", descr = (StringT, False)}

enabled_column :: CInfo
enabled_column = CInfo {cname = "enabled", descr = (BoolT, False)}

capability_column :: CInfo
capability_column = CInfo {cname = "capability", descr = (StringT, False)}

main :: IO ()
main = do
  -- Generate the Haskell module for use in the application
  dbInfoToModuleFiles "" "Database" database

  -- Generate the sqlite3 database
  sqliteConnect "website.db" $ \db -> dbSpecToDatabase db database
