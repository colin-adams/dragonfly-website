module Dragonfly.Authorization.Registration (
                     handleRegistration,
                     handleLogin
                     ) where

import Control.Applicative
import Control.Applicative.Error
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

type XForm a = F.XHtmlForm IO a

data Registration = Registration { regUser :: String
                                 , regPass :: String }

cookieExpirationTime :: Seconds
cookieExpirationTime = 3600 * 24 * 7 -- 1 week

handleLogin :: MyServerPartT Response
handleLogin = do
  ApplicationState db _ <- lift get
  withForm  loginURL (login db) showErrorsInline completeLogin

handleRegistration :: MyServerPartT Response
handleRegistration = do
  ApplicationState db _ <- lift get
  withForm registerURL (register db) showErrorsInline completeRegistration

withForm :: String -> XForm a -> (X.Html -> [String] -> MyServerPartT Response) -> (a -> MyServerPartT Response) -> MyServerPartT Response 
withForm name frm handleErrors handleOk = dir (tail name) $ msum
  [ methodSP GET $ createForm [] frm >>= okHtml
  , withDataFn lookPairs $ \d ->
      methodSP POST $ handleOk' $ simple d
  ]
  where
    handleOk' d = do
      let (extractor, html, _) = runFormState d frm
      v <- liftIO extractor  
      case v of
        Failure faults -> do 
          f <- createForm d frm
          handleErrors f faults
        Success s      -> handleOk s
    simple d = List.map (\(k,v) -> (k, Left v)) d
 
showErrorsInline :: X.Html -> [String] -> MyServerPartT Response
showErrorsInline renderedForm errors =
  okHtml $ X.toHtml (show errors) +++ renderedForm
 
createForm :: Env -> XForm a -> MyServerPartT X.Html
createForm env frm = do
  let (extractor, xml, endState) = runFormState env frm
  xml' <- liftIO xml
  return $ X.form X.! [X.method "POST"] << (xml' +++ X.submit "submit" "Submit")
 
okHtml :: (X.HTML a) => a -> MyServerPartT Response
okHtml content = ok $ toResponse $ htmlPage $ content
 
htmlPage :: (X.HTML a) => a -> X.Html
htmlPage content = (X.header << (X.thetitle << "Colin's dragonflies"))
  +++ (X.body << content)

login :: Database -> XForm Registration
login db = Registration <$> (loginUser db) <*> (pass "Password")

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
  let p = encryptPassword (regPass reg)
  found <- liftIO $ userPasswordMatches u p db
  case found of
    True -> do
      groups <- liftIO $ groupsForUser u db
      signIn reg groups
      rq <- askRq
      let c = lookup "_cont" (rqInputs rq)
      let cont = case c of 
                   Just (Input c' _ _) -> map (chr . fromIntegral) (unpack c')
                   Nothing -> ""
      okHtml $ landingPage u cont
    False -> okHtml $ X.p << ("Password not validated for user name " ++ u)

register :: Database -> XForm Registration
register db = Registration <$> (registerUser db) <*> passConfirmed

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
          restrict (t!userName .==. constant u .&&. t!password .==. constant p)
          return t
    rs <- query db q
    return $ length rs == 1

groupsForUser :: String -> Database -> IO [String]
groupsForUser u db = do
  let q = do
        t <- table UA.userAuthTable
        restrict (t!UA.userName .==. constant u)
        return t
  rs <- query db q
  return $ map (\row -> row!UA.authName) rs

completeRegistration :: Registration -> MyServerPartT Response
completeRegistration reg = do
  ApplicationState db _ <- lift get
  let u = regUser reg
  let p = encryptPassword (regPass reg)
  liftIO $ DB.transaction db (DB.insert db userTable (userName <<- u # password <<- p # enabled <<- False))
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

signIn :: Registration -> [String] -> MyServerPartT ()
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
