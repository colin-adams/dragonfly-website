module Dragonfly.Application (
                    showRootPage,
                    handleRoot,
                    handleSignOut
                    ) where

import Control.Monad

import Data.ByteString.Lazy (unpack)
import Data.Char (chr)

import Text.XHtml
import Text.XHtml.Strict

import Happstack.Server
import Happstack.Helpers

import Dragonfly.ApplicationState
import Dragonfly.URISpace
import Dragonfly.ImageGallery.ImageGallery (divImageGallery)
import Dragonfly.ImageGallery.Upload (divImageUpload)

handleRoot :: MyServerPartT Response
handleRoot = do
  rq <- askRq
  let paths = rqPaths rq
  if null paths then do
                  let cookies = rqCookies rq
                  let sc = lookup sessionCookie cookies
                  let msg = case lookup "_message" (rqInputs rq) of
                              Just (Input msg' _ _) -> thediv ! [theclass "message"] << thespan << map (chr . fromIntegral) (unpack msg')
                              Nothing -> thediv << noHtml
                  rootPage sc msg
     else mzero

handleSignOut :: MyServerPartT Response
handleSignOut = exactdir signOutURL $  do
               addCookie 0 (mkCookie sessionCookie "0")
               rootPage Nothing (thediv << noHtml)

titleText :: String
titleText = "Colin's dragonflies"

showRootPage :: Bool -> Html -> MyServerPartT Response
showRootPage loggedIn msg = do
  let divCont = if loggedIn then signOutDiv else loginRegisterDiv
  return $ toResponse $ (header << (thetitle << titleText) +++ stylesheet) +++
             (body << ((h1 << titleText) +++ msg +++ divCont +++ 
                                         divImageGallery +++ divImageUpload))

rootPage :: Maybe Cookie -> Html -> MyServerPartT Response
rootPage sc msg = do
  -- Actually we should check the session and expire the cookie if not present
  let loggedIn = case sc of
                   Nothing -> False
                   Just c -> True
  showRootPage loggedIn msg

loginRegisterDiv :: Html
loginRegisterDiv = thediv << ((anchor ! [href $ loginURL ++ "?_cont=/"] << "login") 
                   +++ " | " 
                   +++ (anchor ! [href $ registerURL  ++ "?_cont=/"] << "register"))

signOutDiv :: Html
signOutDiv = thediv << (anchor ! [href signOutURL] << "sign out")

stylesheet :: Html
stylesheet = style << "div.message { color: red; }"

