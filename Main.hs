module Main where

import Control.Monad
import Control.Monad.Reader
import Database.HaskellDB.HDBC.SQLite3
import Happstack.Server

import Dragonfly.Authorization.Registration
import Dragonfly.ApplicationState
import Dragonfly.Application
import Dragonfly.ImageGallery.ImageGallery (handleImageGallery)
import Dragonfly.ImageGallery.Upload (handleImageUpload)

main :: IO ()
main = sqliteConnect "website.db" $ \db -> do
         st <- initialState db
         simpleHTTP' (\action -> runReaderT action st) (nullConf {port = 9959}) (msum [handleRoot, handleRegistration, handleLogin, handleSignOut, handleImageGallery, handleImageUpload]) 
