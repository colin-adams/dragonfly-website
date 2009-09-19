-- | Authorization functioms
module Dragonfly.Authorization.Auth (
                                     withSession
                                    ) where

import Control.Applicative.State
import Control.Monad
import Control.Monad.Reader

import Happstack.Server

import Dragonfly.ApplicationState
import Dragonfly.Authorization.User

-- | Run f only when logged in
withSession :: (User -> MyServerPartT a) -> MyServerPartT a -> MyServerPartT a
withSession f guestSPT = withSessionId action
  where action (Just sid) = findSession sid >>= maybe noSession f
        action Nothing = guestSPT
        noSession = clearSessionCookie >> guestSPT

-- | Get session identifier from cookies
getSessionId  :: (Read a) => Control.Monad.Reader.ReaderT ([(String, Input)], [(String, Cookie)]) Maybe (Maybe a)
getSessionId = liftM Just (readCookieValue sessionCookie) `mplus` return Nothing

-- | Run action with a possible logged-in user
withSessionId :: (Read a, MonadPlus m, ServerMonad m) => (Maybe a -> m r) -> m r
withSessionId = withDataFn getSessionId

-- | Remove any expired session cookie
clearSessionCookie :: MyServerPartT ()
clearSessionCookie = addCookie 0 (mkCookie sessionCookie "0")

-- | Find user from session cookie value
findSession :: String -> MyServerPartT (Maybe User)
findSession sid = do
  ApplicationState _ sessions <- lift get
  return $ session sessions (SessionKey (read sid))
