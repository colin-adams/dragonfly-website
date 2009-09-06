{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction,
TemplateHaskell , FlexibleInstances,
UndecidableInstances, OverlappingInstances,
MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
 
module Auth where
 
import Char
import Maybe
import Numeric
import Random
 
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State (modify,put,get,gets)
import Control.Monad.Trans
import Codec.Utils
import Data.ByteString.Internal
import Data.Digest.SHA512
import Data.Generics hiding ((:+:))
import Data.Word
import Happstack.Data
import Happstack.Data.IxSet
import Happstack.Server
import Happstack.State
 
sessionCookie = "sid"
 
newtype SessionKey = SessionKey Integer deriving (Read,Show,Ord,Eq,Typeable,Data,Num,Random)
instance Version SessionKey
$(deriveSerialize ''SessionKey)
 
newtype UserId = UserId { unUid :: Word64 } deriving (Read,Show,Ord,Eq,Typeable,Data,Num)
instance Version UserId
$(deriveSerialize ''UserId)
 
newtype Username = Username { unUser :: String } deriving (Read,Show,Ord,Eq,Typeable,Data)
instance Version Username
$(deriveSerialize ''Username)
 
data SessionData = SessionData {
  sesUid :: UserId,
  sesUsername :: Username
} deriving (Read,Show,Eq,Typeable,Data)
 
newtype SaltedHash = SaltedHash [Octet] deriving (Read,Show,Ord,Eq,Typeable,Data)
instance Version SaltedHash
$(deriveSerialize ''SaltedHash)
 
saltLength = 16
strToOctets = listToOctets . (map c2w)
slowHash a = (iterate hash a) !! 512
randomSalt :: IO String
randomSalt = liftM concat $ sequence $ take saltLength $ repeat $
  randomRIO (0::Int,15) >>= return . flip showHex ""
buildSaltAndHash :: String -> IO SaltedHash
buildSaltAndHash str = do
  salt <- randomSalt
  let salt' = strToOctets salt
  let str' = strToOctets str
  let h = slowHash (salt'++str')
  return $ SaltedHash $ salt'++h
checkSalt :: String -> SaltedHash -> Bool
checkSalt str (SaltedHash h) = h == salt++(slowHash $ salt++(strToOctets str))
  where salt = take saltLength h
  
data Sessions a = Sessions {unsession::M.Map SessionKey a}
  deriving (Read,Show,Eq,Typeable,Data)
 
data User = User {
  userid :: UserId,
  username :: Username,
  userpass :: SaltedHash
} deriving (Read,Show,Ord,Eq,Typeable,Data)
 
$(inferIxSet "UserDB" ''User 'noCalcs [''UserId, ''Username])
 
data AuthState = AuthState {
sessions :: Sessions SessionData,
users :: UserDB,
nextUid :: UserId
} deriving (Show,Read,Typeable,Data)
instance Version SessionData
instance Version (Sessions a)
 
$(deriveSerialize ''SessionData)
$(deriveSerialize ''Sessions)
 
instance Version AuthState
instance Version User
 
$(deriveSerialize ''User)
$(deriveSerialize ''AuthState)
 
instance Component AuthState where
type Dependencies AuthState = End
initialValue = AuthState (Sessions M.empty) empty 0
 
askUsers :: Query AuthState UserDB
askUsers = return . users =<< ask
 
askSessions :: Query AuthState (Sessions SessionData)
askSessions = return . sessions =<< ask
 
getUser :: Username -> Query AuthState (Maybe User)
getUser username = do
udb <- askUsers
return $ getOne $ udb @= username
 
getUserById :: UserId -> Query AuthState (Maybe User)
getUserById uid = do
udb <- askUsers
return $ getOne $ udb @= uid
 
modUsers :: (UserDB -> UserDB) -> Update AuthState ()
modUsers f = modify (\s -> (AuthState (sessions s) (f $ users s) (nextUid s)))
 
modSessions :: (Sessions SessionData -> Sessions SessionData) -> Update AuthState ()
modSessions f = modify (\s -> (AuthState (f $ sessions s) (users s) (nextUid s)))
 
getAndIncUid :: Update AuthState UserId
getAndIncUid = do
uid <- gets nextUid
modify (\s -> (AuthState (sessions s) (users s) (uid+1)))
return uid
 
isUser :: Username -> Query AuthState Bool
isUser name = do
us <- askUsers
return $ isJust $ getOne $ us @= name
 
addUser :: Username -> SaltedHash -> Update AuthState (Maybe User)
addUser name pass = do
s <- get
let exists = isJust $ getOne $ (users s) @= name
if exists
then return Nothing
else do u <- newUser name pass
modUsers $ insert u
return $ Just u
where newUser u p = do uid <- getAndIncUid
return $ User uid u p
 
delUser :: Username -> Update AuthState ()
delUser name = modUsers del
where del db = case getOne (db @= name) of
Just u -> delete u db
Nothing -> db
 
updateUser u = do modUsers (updateIx (userid u) u)
 
authUser :: String -> String -> Query AuthState (Maybe User)
authUser name pass = do
udb <- askUsers
let u = getOne $ udb @= (Username name)
case u of
(Just v) -> return $ if checkSalt pass (userpass v) then u else Nothing
Nothing -> return Nothing
 
listUsers :: Query AuthState [Username]
listUsers = do
udb <- askUsers
return $ map username $ toList udb
 
numUsers :: Query AuthState Int
numUsers = liftM length listUsers
 
setSession :: SessionKey -> SessionData -> Update AuthState ()
setSession key u = do
modSessions $ Sessions . (M.insert key u) . unsession
return ()
 
newSession u = do
key <- getRandom
setSession key u
return key
 
delSession :: SessionKey -> Update AuthState ()
delSession key = do
modSessions $ Sessions . (M.delete key) . unsession
return ()
 
clearAllSessions :: Update AuthState ()
clearAllSessions = modSessions $ const (Sessions M.empty)
 
getSession :: SessionKey -> Query AuthState (Maybe SessionData)
getSession key = liftM ((M.lookup key) . unsession) askSessions
 
getSessions :: SessionKey -> Query AuthState (Sessions SessionData)
getSessions key = askSessions
 
numSessions:: Query AuthState Int
numSessions = liftM (M.size . unsession) askSessions
 
$(mkMethods ''AuthState ['askUsers, 'addUser, 'getUser, 'getUserById, 'delUser, 'authUser,
'isUser, 'listUsers, 'numUsers, 'updateUser, 'clearAllSessions,
             'setSession, 'getSession, 'getSessions, 'newSession, 'delSession, 'numSessions])
 
{-
- Login page
-}
 
data UserAuthInfo = UserAuthInfo String String
instance FromData UserAuthInfo where
  fromData = liftM2 UserAuthInfo (look "username")
             (look "password" `mplus` return "nopassword")
 
performLogin user = do
  key <- update $ NewSession (SessionData (userid user) (username user))
  addCookie (2678400) (mkCookie sessionCookie (show key))
 
{-
- Handles data from a login form to log the user in. The form must supply
- fields named "username" and "password".
-}
loginHandler successResponse failResponse = withData handler
  where handler (UserAuthInfo user pass) = do
          mu <- query $ AuthUser user pass
          case mu of
            Just u -> do performLogin u
                         successResponse
            Nothing -> failResponse
 
{-
- Logout page
-}
 
performLogout sid = do
  clearSessionCookie
  update $ DelSession sid
 
logoutHandler target = withSessionId handler
  where handler (Just sid) = do
          performLogout sid
          target
        handler Nothing = target
 
{-
- Registration page
-}
 
data NewUserInfo = NewUserInfo String String String
instance FromData NewUserInfo where
  fromData = liftM3 NewUserInfo (look "username")
             (look "password" `mplus` return "nopassword")
             (look "password2" `mplus` return "nopassword2")
 
register user pass = do
  h <- liftIO $ buildSaltAndHash pass
  update $ AddUser user h
 
checkAndAdd uExists good user pass = do
  u <- register user pass
  case u of
    Just u' -> do performLogin u'
                  good
    Nothing -> uExists
 
newUserHandler existsOrInvalid nomatch succ = newUserHandler' existsOrInvalid nomatch (const succ)
 
{- newUserHandler' passes the username of just created account to
- the success part. This can be used to initiate any data associated
- with a user.
-}
newUserHandler' existsOrInvalid nomatch succ = withData handler
  where handler (NewUserInfo user pass1 pass2)
          | not (saneUsername user) = existsOrInvalid
          | pass1 /= pass2 = nomatch
          | otherwise = checkAndAdd existsOrInvalid (succ (Username user)) (Username user) pass1
        saneUsername str = foldl1 (&&) $ map isAlphaNum str
 
 
{-
- Handles data from a new user registration form. The form must supply
- fields named "username", "password", and "password2".
-}
newAccountHandler noMatch uExists good (NewUserInfo user pass1 pass2)
  | pass1 == pass2 = checkAndAdd uExists good (Username user) pass1
  | otherwise = noMatch
 
changePassword :: (MonadIO m)
               => String
               -> String
               -> String
               -> m Bool
changePassword user oldpass newpass = do
  mu <- query $ AuthUser user oldpass
  case mu of
    (Just u) -> do h <- liftIO $ buildSaltAndHash newpass
                   update $ UpdateUser (u {userpass = h})
                   return True
    Nothing -> return False
 
{-
- Requiring a login
-}
 
clearSessionCookie = addCookie 0 (mkCookie sessionCookie "0")
 
getSessionId = liftM Just (readCookieValue sessionCookie) `mplus` return Nothing
 
withSessionId = withDataFn getSessionId
 
getLoggedInUser = withSessionId action
  where action (Just sid) = query $ GetSession sid
        action Nothing = return Nothing
 
withSession :: (MonadIO m)
            => (SessionData -> ServerPartT m a)
            -> ServerPartT m a
            -> ServerPartT m a
withSession f guestSPT = withSessionId action
  where action (Just sid) = (query $ GetSession sid) >>= (maybe noSession f)
        action Nothing = guestSPT
        noSession = clearSessionCookie >> guestSPT
 
loginGate :: (MonadIO m)
          => ServerPartT m a
          -> ServerPartT m a
          -> ServerPartT m a
loginGate reg guest = withSession (\_ -> reg) guest
