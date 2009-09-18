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

withForm :: String -> XForm a -> (X.Html -> [String] -> MyServerPartT Response) -> (a -> MyServerPartT Response) -> MyServerPartT Response 
withForm name frm handleErrors handleOk = dir (tail name) $ msum
  [ methodSP GET $ createForm [] frm >>= okHtml
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
 
showErrorsInline :: X.Html -> [String] -> MyServerPartT Response
showErrorsInline renderedForm errors =
  okHtml $ X.toHtml (show errors) +++ renderedForm
 
createForm :: Env -> XForm a -> MyServerPartT X.Html
createForm env frm = do
  let (extractor, xml, endState) = runFormState env frm
  xml' <- liftIO xml
  return $ X.form X.! [X.method "POST"] << (xml' +++ X.submit "submit" "Submit")
 
okHtml :: (X.HTML a) => a -> MyServerPartT Response
okHtml = ok . toResponse . htmlPage
 
htmlPage :: (X.HTML a) => a -> X.Html
htmlPage content = (X.header << (X.thetitle << "Colin's dragonflies"))
  +++ (X.body << content)
