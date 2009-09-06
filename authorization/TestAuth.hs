module Main where

import Control.Monad
import Control.Monad.State
import Database.HaskellDB.HDBC.SQLite3
import Registration
import Happstack.Server
import ApplicationState
import Application

main :: IO ()
main = do
  sqliteConnect "website.db" $ \db -> do
         let st = initialState db
         simpleHTTP' (\action -> evalStateT action st) (nullConf {port = 9959}) (msum [handleRoot, handleRegistration, handleLogin, handleSignOut]) 

  


