module Main where

import Control.Monad
import Control.Monad.Reader
import Database.HaskellDB.HDBC.PostgreSQL
import Happstack.Server

import Dragonfly.Authorization.Registration
import Dragonfly.ApplicationState
import Dragonfly.Application
import Dragonfly.ImageGallery.ImageGallery (handleImageGallery, handleImages)
import Dragonfly.ImageGallery.Upload (handleImageUpload)

main :: IO ()
main = postgresqlConnect [] $ \db -> do
         st <- initialState db
         simpleHTTP' (\action -> runReaderT action st) (nullConf {port = 9959}) (msum [handleRoot, handleRegistration, handleLogin, handleSignOut, handleImageGallery, handleImageUpload, handleImages]) 
