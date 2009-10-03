-- | XHtml form handling
module Dragonfly.Forms where

import Control.Applicative.Error
import Control.Applicative.State
import Control.Arrow (second)

import Data.List as List

import Happstack.Server

import Text.Formlets
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict.Formlets as F

import Dragonfly.ApplicationState

type XForm a = F.XHtmlForm IO a

-- | Process form if URL path matches name
withForm :: String -> XForm a -> (X.Html -> [String] -> MyServerPartT Response) -> (a -> MyServerPartT Response) -> MyServerPartT Response 
withForm name frm handleErrors handleOk = dir (tail name) $ msum
  [methodSP GET $ createForm [] frm >>= okHtml
  , withDataFn lookPairs $ methodSP POST . handleOk' . simple
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
    simple = List.map (second Left)

-- | Prepend errors to the rendered form 
showErrorsInline :: X.Html -> [String] -> MyServerPartT Response
showErrorsInline renderedForm errors = okHtml $ X.toHtml (show errors) +++ renderedForm
 
-- | Display a form to the user with a submit button
createForm :: Env -> XForm a -> MyServerPartT X.Html
createForm env frm = do
  let (extractor, xml, endState) = runFormState env frm
  xml' <- liftIO xml
  return $ X.form X.! [X.method "POST", X.enctype "multipart/form-data"] << (xml' +++ X.submit "submit" "Submit")
 
-- | Display a form to the user with a preview button
createPreview :: Env -> XForm a -> MyServerPartT X.Html
createPreview env frm = do
  let (extractor, xml, endState) = runFormState env frm
  xml' <- liftIO xml
  return $ X.form X.! [X.method "POST", X.enctype "multipart/form-data"] << (xml' +++ X.submit "preview" "Preview")
 
-- | Render an html page as a good response
okHtml :: (X.HTML a) => a -> MyServerPartT Response
okHtml = ok . toResponse . htmlPage
 
-- | Render content within a standatd html template
htmlPage :: (X.HTML a) => a -> X.Html
htmlPage content = (X.header << (X.thetitle << "Colin's dragonflies"))
  +++ (X.body << content)
