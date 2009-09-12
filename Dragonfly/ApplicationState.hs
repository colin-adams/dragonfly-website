{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dragonfly.ApplicationState (
                         ApplicationState (..),
                         MyServerPartT,
                         initialState,
                         sessionCookie,
                         newSession
                         ) where

import Database.HaskellDB.Database (Database)
import Happstack.Server.SimpleHTTP (ServerPartT)
import Control.Monad.State
import System.Random
import qualified Data.Map as M

newtype SessionKey = SessionKey Integer deriving (Read, Show, Ord, Eq, Num, Random)

sessionCookie :: String
sessionCookie = "sid"

data SessionData = SessionData {sesUsername :: String, groups :: [String]}  deriving (Read, Show, Eq)

data Sessions = Sessions {unsession::M.Map SessionKey SessionData}
  deriving (Read, Show, Eq)
 
type MyServerPartT = ServerPartT (StateT ApplicationState IO)

data ApplicationState = ApplicationState {
      db :: !Database,
      sessions :: Sessions
}

initialState :: Database -> ApplicationState
initialState db = ApplicationState db (Sessions M.empty)

newSession :: String -> [String] -> SessionKey -> ApplicationState -> ApplicationState
newSession u g k st =
    let s = sessions st 
        m = unsession s
        m' = M.insert k (SessionData u g) m
        s' = s {unsession = m'}
    in st {sessions = s'}
