-- | Program to initialize database tables for a new system.
-- | First run DatabaseDefinitions.
-- | Then compile this program.
-- | Then run it, supplying a name and password for the (or a principal) adminstrator
module Main (main) where

import Database.HaskellDB.HDBC.SQLite3
import Database.HaskellDB
import Database.HaskellDB.Database
import Database.User_table
import Database.Auth_table
import qualified Database.User_auth_table as UA
import Database.Capabilities_table
import qualified Database.Auth_capabilities_table as AC

import Dragonfly.Authorization.Authorities
import Dragonfly.Authorization.Password

import System.Environment
import System.Console.GetOpt

-- | Run the initialization program.
main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  let (options, non_options, errors) = getOpt RequireOrder optionDescriptions args
  case null errors of
    True -> case null non_options of
              True -> initialize options
              False -> usage errors
    False -> usage errors

-- | Report errors and usage message
usage :: [String] -> IO ()
usage errors = do
  mapM putStrLn errors
  putStrLn $ usageInfo "\nUsage: Initialize database, passing name and password for principal adminstrator" optionDescriptions

data Options = Options {name :: String, passwd :: String} deriving Show

defaultOptions :: Options
defaultOptions = Options {name = "", passwd = ""}

-- | Initialize the database.
initialize :: [Options -> Options] -> IO ()
initialize options = do
  case length options of
    2 -> do
      let opts = foldl (flip id) defaultOptions options
      sqliteConnect "website.db" $ \db -> do
                        addAdministrator (name opts) (passwd opts) db
                        return ()
    _ -> usage ["Exactly 2 options must be supplied"]

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = 
    [
     Option ['n'] ["name"] (ReqArg (\n opts -> opts {name = n}) "Name") "Name of principal administartor",
     Option ['p'] ["password"] (ReqArg (\p opts -> opts {passwd = p}) "Password") "Password (not encrypyted) for principal administartor"
    ]

addAdministrator :: String -> String -> Database -> IO ()
addAdministrator user pass db = do
  let p = encryptPassword pass
  transaction db $ do
    insert db auth_table (auth_name <<- administratorAuthority)
    insert db UA.user_auth_table (UA.user_name <<- user # UA.auth_name <<- administratorAuthority)
    mapM (\c -> insert db capabilities_table (capability <<- c)) allCapabilities
    mapM (\c -> insert db AC.auth_capabilities_table (AC.auth_name <<- administratorAuthority # AC.capability <<- c)) allCapabilities
    insert db user_table (user_name <<- user # password <<- p # enabled <<- True)

           

