module Dragonfly.ApplicationState (
                                   Sessions,
                                   ApplicationState (..),
                                   MyServerPartT,
                                   initialState,
                                   sessionCookie,
                                   newSession,
                                   session
                                  ) where

import Database.HaskellDB.Database (Database)
import Happstack.Server.SimpleHTTP (ServerPartT)
import Control.Monad.State
import qualified Data.Map as M
import Dragonfly.Authorization.User
import Dragonfly.Authorization.Group

sessionCookie :: String
sessionCookie = "sid"

data Sessions = Sessions {unsession::M.Map SessionKey User}
  deriving (Read, Show, Eq)
 
type MyServerPartT = ServerPartT (StateT ApplicationState IO)

data ApplicationState = ApplicationState {
      db :: !Database,
      sessions :: Sessions
}

session :: Sessions -> SessionKey -> Maybe User
session sessions key =
    M.lookup key (unsession sessions)

initialState :: Database -> ApplicationState
initialState db = ApplicationState db (Sessions M.empty)

newSession :: String -> [(String, Group)] -> SessionKey -> ApplicationState -> ApplicationState
newSession u g k st =
    let s = sessions st 
        m = unsession s
        m' = M.insert k (User u k g) m
        s' = s {unsession = m'}
    in st {sessions = s'}
