{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ApplicationState (
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

newtype SessionData = SessionData {sesUsername :: String}  deriving (Read, Show, Eq)

data Sessions = Sessions {unsession::M.Map SessionKey SessionData}
  deriving (Read, Show, Eq)
 
type MyServerPartT = ServerPartT (StateT ApplicationState IO)

data ApplicationState = ApplicationState {
      db :: !Database,
      sessions :: Sessions
}

initialState :: Database -> ApplicationState
initialState db = ApplicationState db (Sessions M.empty)

newSession :: String -> SessionKey -> ApplicationState -> ApplicationState
newSession u k st =
    let s = sessions st 
        m = unsession s
        m' = M.insert k (SessionData u) m
        s' = s {unsession = m'}
    in st {sessions = s'}
