module Dragonfly.ApplicationState (
                                   Sessions,
                                   ApplicationState (..),
                                   MyServerPartT,
                                   initialState,
                                   sessionCookie,
                                   newSession,
                                   session
                                  ) where

import Control.Concurrent.MVar
import Control.Monad.Reader

import qualified Data.Map as M

import Database.HaskellDB.Database (Database)

import Happstack.Server.SimpleHTTP (ServerPartT)

import Dragonfly.Authorization.User
import Dragonfly.Authorization.Group

-- | Name of our session cookie
sessionCookie :: String
sessionCookie = "sid"

-- | Session information (held in an MVar)
data Sessions = Sessions {unsession::M.Map SessionKey User}
  deriving (Read, Show, Eq)
 
-- | Our monad
type MyServerPartT = ServerPartT (ReaderT ApplicationState IO)

-- | State shared by all request applications (threads)
data ApplicationState = ApplicationState {
      db :: !Database,
      sessions :: MVar Sessions
}

-- | Lookup logged-in user from session key (carried in session cookie)
session :: Sessions -> SessionKey -> Maybe User
session sessions key = M.lookup key (unsession sessions) 

-- | State when webserver starts
initialState :: Database -> IO ApplicationState
initialState db = do
  sess <- newMVar $ Sessions M.empty
  return $ ApplicationState db  sess

-- | Create and save a new session, given a session key
newSession :: String -> [(String, Group)] -> SessionKey -> MyServerPartT ()
newSession u g k = do
  ApplicationState _ sess <- ask
  s <- liftIO $ takeMVar $ sess 
  let m = unsession s
      m' = M.insert k (User u k g) m
      s' = s {unsession = m'}
  liftIO $ putMVar sess s'
