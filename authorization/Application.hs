module Application (
                    showRootPage,
                    handleRoot,
                    handleSignOut
                    ) where

import Text.XHtml
import Text.XHtml.Strict
import Happstack.Server
import Happstack.Helpers
import ApplicationState
import URISpace

handleRoot :: MyServerPartT Response
handleRoot = exactdir "/" $  do
               rq <- askRq
               let cookies = rqCookies rq
               let sq = lookup sessionCookie cookies
               rootPage sq

handleSignOut :: MyServerPartT Response
handleSignOut = exactdir signOutURL $  do
               addCookie 0 (mkCookie sessionCookie "0")
               rootPage Nothing

titleText :: String
titleText = "Colin's dragonflies"

showRootPage :: Bool -> MyServerPartT Response
showRootPage loggedIn = do
  let divCont = case loggedIn of
                 False -> loginRegisterDiv
                 True -> signOutDiv
  return $ toResponse $ (header << thetitle << titleText) +++
             (body << ((h1 << titleText) +++ divCont))

rootPage :: Maybe Cookie -> MyServerPartT Response
rootPage sc = do
  let loggedIn = case sc of
                   Nothing -> False
                   Just c -> True
  showRootPage loggedIn

loginRegisterDiv :: Html
loginRegisterDiv = thediv << ((anchor ! [href $ loginURL ++ "?_cont=/"] << "login") 
                   +++ " | " 
                   +++ (anchor ! [href $ registerURL  ++ "?_cont=/"] << "register"))

signOutDiv :: Html
signOutDiv = thediv << (anchor ! [href signOutURL] << "sign out")

