module Dragonfly.Authorization.Registration (
                     handleRegistration,
                     handleLogin,
                     groupsForUser
                     ) where

import Control.Applicative
import Control.Applicative.State

import Data.ByteString.Lazy (unpack)
import Data.Char (chr)
import Data.List as List

import Database.HaskellDB hiding ((<<))
import Database.HaskellDB.Database as DB
import Database.UserTable
import qualified Database.UserAuthTable as UA

import Happstack.Server
import Happstack.Server.HTTP.Types
import Happstack.Helpers
import Happstack.Util.Common (Seconds)

import System.Random

import Text.Formlets
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict.Formlets as F

import Dragonfly.URISpace
import Dragonfly.ApplicationState
import Dragonfly.Application
import Dragonfly.Authorization.Password
import Dragonfly.Authorization.Group
import Dragonfly.Authorization.User
import Dragonfly.Authorization.Auth
import Dragonfly.Forms

data Registration = Registration { regUser :: String
                                 , regPass :: String }

cookieExpirationTime :: Seconds
cookieExpirationTime = 2 * 3600  -- 2 hours

handleLogin :: MyServerPartT Response
handleLogin = do
  ApplicationState db _ <- lift get
  withForm  loginURL (login db) showErrorsInline completeLogin

handleRegistration :: MyServerPartT Response
handleRegistration = do
  ApplicationState db _ <- lift get
  withForm registerURL (register db) showErrorsInline completeRegistration

login :: Database -> XForm Registration
login db = Registration <$> loginUser db <*> pass "Password"

loginUser :: Database -> XForm String
loginUser db = input `F.checkM` F.ensureM valid error where
    input = "Username" `label` F.input Nothing
    valid name = do 
      missing <- userAbsent name db
      return $ not missing
    error = "Username not recognised."

completeLogin :: Registration -> MyServerPartT Response
completeLogin reg = do
  ApplicationState db _ <- lift get
  let u = regUser reg
  let p = regPass reg
  found <- liftIO $ userPasswordMatches u p db
  if found then
    do
      g <- liftIO $ groupsForUser u db
      groups <- liftIO $ mapM (newGroup db) g
      signIn reg groups
      rq <- askRq
      let c = lookup "_cont" (rqInputs rq)
      let cont = case c of 
                   Just (Input c' _ _) -> map (chr . fromIntegral) (unpack c')
                   Nothing -> ""
      okHtml $ landingPage u cont
    else okHtml $ X.p << ("Password not validated for user name " ++ u)

register :: Database -> XForm Registration
register db = Registration <$> registerUser db <*> passConfirmed

registerUser :: Database -> XForm String
registerUser db = pureRegisterUser `F.checkM` F.ensureM valid error where
    valid name = userAbsent name db
    error = "Username already exists in the database!"

userAbsent :: String -> Database -> IO Bool
userAbsent u db = do
  let q = do
        t <- table userTable
        restrict (t!userName .==. constant u)
        return t
  rs <- query db q
  return $ null rs

userPasswordMatches :: String -> String -> Database -> IO Bool
userPasswordMatches u p db = do
  let q = do
        t <- table userTable
        restrict (t!userName .==. constant u)
        return t
  rs <- query db q
  if length rs == 1 && checkSalt p  (stringToSalt ((head rs) ! password)) then
      return True
      else return False

-- | All groups which user belongs to
groupsForUser :: String -> Database -> IO [String]
groupsForUser u db = do
  let q = do
        t <- table UA.userAuthTable
        restrict (t!UA.userName .==. constant u)
        return t
  rs <- query db q
  return $ map (!UA.authName) rs

completeRegistration :: Registration -> MyServerPartT Response
completeRegistration reg = do
  ApplicationState db _ <- lift get
  let u = regUser reg
  p <- liftIO $ buildSaltAndHash (regPass reg)
  liftIO $ DB.transaction db (DB.insert db userTable (userName <<- u # password <<- (saltToString p) # enabled <<- False))
  signIn reg []
  rq <- askRq
  let c = lookup "_cont" (rqInputs rq)
  let cont = case c of 
               Just (Input c' _ _) -> map (chr . fromIntegral) (unpack c')
               Nothing -> ""
  okHtml $ registeredPage u cont
         
pureRegisterUser :: XForm String
pureRegisterUser = input `F.check` F.ensure valid error where
    input = "Username" `label` F.input Nothing
    valid = (>= 3) . length
    error = "Username must be three characters or longer."

passConfirmed :: XForm String
passConfirmed = fst <$> passwords `F.check` F.ensure equal error where
    passwords = (,) <$> pass "Password" <*> pass "Password (confirm)"
    equal (a, b) = a == b
    error = "The entered passwords do not match!"

pass :: String -> XForm String
pass caption = input `F.check` F.ensure valid error where
    input = caption `label` F.password Nothing
    valid = (>=6) . length
    error = "Password must be six characters or longer."

label :: String -> XForm String -> XForm String
label l = F.plug (\xhtml -> X.p << (X.label << (l ++ ": ") +++ xhtml))

signIn :: Registration -> [(String, Group)] -> MyServerPartT ()
signIn reg groups = do
  let u = regUser reg
  key <- liftIO randomIO
  lift $ modify $ newSession u groups key
  let c = mkCookie sessionCookie (show key)
  addCookie cookieExpirationTime c

registeredPage :: String -> String -> X.Html
registeredPage u continuation = 
    (X.header << (X.thetitle << "Colin's dragonflies"))
    +++ (X.body << (X.p << (u ++ " is successfully registered. The webmaster will enable your access in the near future")) +++
        X.anchor X.! [X.href continuation] << "continue")

landingPage :: String -> String -> X.Html
landingPage u continuation = 
    (X.header << (X.thetitle << "Colin's dragonflies"))
    +++ (X.body << (X.p << (u ++ " is now logged in.")) +++
        X.anchor X.! [X.href continuation] << "continue")
