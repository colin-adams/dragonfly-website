module Dragonfly.Application (
                    showRootPage,
                    handleRoot,
                    handleSignOut
                    ) where

import Text.XHtml
import Text.XHtml.Strict
import Happstack.Server
import Happstack.Helpers
import Dragonfly.ApplicationState
import Dragonfly.URISpace
import Dragonfly.ImageGallery.ImageGallery

handleRoot :: MyServerPartT Response
handleRoot = exactdir "/" $  do
               rq <- askRq
               let cookies = rqCookies rq
               let sc = lookup sessionCookie cookies
               rootPage sc

handleSignOut :: MyServerPartT Response
handleSignOut = exactdir signOutURL $  do
               addCookie 0 (mkCookie sessionCookie "0")
               rootPage Nothing

titleText :: String
titleText = "Colin's dragonflies"

showRootPage :: Bool -> MyServerPartT Response
showRootPage loggedIn = do
  let divCont = if loggedIn then signOutDiv else loginRegisterDiv
  return $ toResponse $ (header << thetitle << titleText) +++
             (body << ((h1 << titleText) +++ divCont +++ divImageGallery))

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


