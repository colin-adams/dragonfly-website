module Main where

import Control.Monad
import Control.Monad.State
import Database.HaskellDB.HDBC.SQLite3
import Happstack.Server

import Dragonfly.Authorization.Registration
import Dragonfly.ApplicationState
import Dragonfly.Application
import Dragonfly.ImageGallery.ImageGallery (handleImageGallery)

main :: IO ()
main = sqliteConnect "website.db" $ \db -> do
         let st = initialState db
         simpleHTTP' (\action -> evalStateT action st) (nullConf {port = 9959}) (msum [handleRoot, handleRegistration, handleLogin, handleSignOut, handleImageGallery]) 
