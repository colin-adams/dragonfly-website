-- | Program to initialize database tables for a new system.
-- | First run DatabaseDefinitions.
-- | Then compile this program.
-- | Then run it, supplying a name and password for the (or a principal) adminstrator
module Main (main) where

import Database.HaskellDB.HDBC.PostgreSQL
import Database.HaskellDB
import Database.HaskellDB.Database
import Database.UserTable
import Database.AuthTable
import qualified Database.UserAuthTable as UA
import Database.CapabilitiesTable
import qualified Database.AuthCapabilitiesTable as AC

import Dragonfly.Authorization.Authorities
import Dragonfly.Authorization.Password

import System.Environment
import System.Console.GetOpt

-- | Run the initialization program.
main :: IO ()
main = do
  args <- getArgs
  let (options, non_options, errors) = getOpt RequireOrder optionDescriptions args
  if null errors && null non_options then
        initialize options
        else usage errors

-- | Report errors and usage message
usage :: [String] -> IO ()
usage errors = do
  mapM_ putStrLn errors
  putStrLn $ usageInfo "\nUsage: Initialize database, passing name, password and email address for principal adminstrator" optionDescriptions

data Options = Options {name :: String, passwd :: String, emailAdr :: String} deriving Show

defaultOptions :: Options
defaultOptions = Options {name = "", passwd = "", emailAdr = ""}

-- | Initialize the database.
initialize :: [Options -> Options] -> IO ()
initialize options = case length options of
    3 -> do
      let opts = foldl (flip id) defaultOptions options
      postgresqlConnect [] $ \db -> do
                        addAdministrator (name opts) (passwd opts) (emailAdr opts) db
                        return ()
    _ -> usage ["Exactly 3 options must be supplied"]

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = 
    [
     Option "n" ["name"] (ReqArg (\n opts -> opts {name = n}) "Name") "Name of principal administrator",
     Option "p" ["password"] (ReqArg (\p opts -> opts {passwd = p}) "Password") "Password (not encrypyted) for principal administrator",
     Option "e" ["email"] (ReqArg (\e opts -> opts {emailAdr = e}) "Email") "Email address for principal administrator"
    ]

addAdministrator :: String -> String -> String -> Database -> IO ()
addAdministrator user pass em db = do
  p <- buildSaltAndHash pass
  transaction db $ do
    insert db authTable (authName <<- administratorAuthority)
    insert db UA.userAuthTable (UA.userName <<- user # UA.authName <<- administratorAuthority)
    mapM_ (insert db capabilitiesTable . (capability <<-)) knownCapabilities
    mapM_ (\c -> insert db AC.authCapabilitiesTable (AC.authName <<- administratorAuthority # AC.capability <<- c)) knownCapabilities
    insert db userTable (userName <<- user # password <<- saltToString p # email <<- em # enabled <<- True)

           

