-- | Authorization functioms
module Dragonfly.Authorization.Auth (
                                     withSession
                                    ) where

import Control.Applicative.State
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader

import Happstack.Server

import Dragonfly.ApplicationState
import Dragonfly.Authorization.User

-- | Run f only when logged in
withSession :: (User -> MyServerPartT a) -> MyServerPartT a -> MyServerPartT a
withSession f guestSPT = do
  rq <- askRq
  let cookies = rqCookies rq
      sc = lookup sessionCookie cookies
  case sc of
    Nothing -> guestSPT
    Just ck -> findSession ck >>= maybe noSession f
    where noSession = clearSessionCookie >> guestSPT

-- | Remove any expired session cookie
clearSessionCookie :: MyServerPartT ()
clearSessionCookie = addCookie 0 (mkCookie sessionCookie "0")

-- | Find user from session cookie
findSession :: Cookie -> MyServerPartT (Maybe User)
findSession sid = do
  ApplicationState _ sessions <- lift ask
  sess <- liftIO $ readMVar sessions
  let key = cookieValue sid
  return $ session sess (read key)
