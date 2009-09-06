module Main where

import Control.Applicative
import Control.Applicative.Error
import Control.Applicative.State
import Data.List as List
import Text.Formlets
import qualified Text.XHtml.Strict.Formlets as F
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++), (<<))
import Happstack.Server

type XForm a = F.XHtmlForm IO a

data Registration = Registration { regUser :: String
                                 , regPass :: String }
                                 deriving Show

handleRegistration :: ServerPartT IO Response
handleRegistration = withForm "register" register showErrorsInline (\u -> okHtml $ regUser u ++ " is successfully registered")

withForm :: String -> XForm a -> (X.Html -> [String] -> ServerPartT IO Response) -> (a -> ServerPartT IO Response) -> ServerPartT IO Response 
withForm name frm handleErrors handleOk = dir name $ msum
  [ methodSP GET $ createForm [] frm >>= okHtml
  , withDataFn lookPairs $ \d ->
      methodSP POST $ handleOk' $ simple d
  ]
  where
    handleOk' d = do
      let (extractor, html, _) = runFormState d "" frm
      v <- liftIO extractor  
      case v of
        Failure faults -> do 
          f <- createForm d frm
          handleErrors f faults
        Success s      -> handleOk s
    simple d = List.map (\(k,v) -> (k, Left v)) d
 
showErrorsInline :: X.Html -> [String] -> ServerPartT IO Response
showErrorsInline renderedForm errors =
  okHtml $ X.toHtml (show errors) +++ renderedForm
 
createForm :: Env -> XForm a -> ServerPartT IO X.Html
createForm env frm = do
  let (extractor, xml, endState) = runFormState env "" frm
  xml' <- liftIO xml
  return $ X.form X.! [X.method "POST"] << (xml' +++ X.submit "submit" "Submit")
 
okHtml :: (X.HTML a) => a -> ServerPartT IO Response
okHtml content = ok $ toResponse $ htmlPage $ content
 
htmlPage :: (X.HTML a) => a -> X.Html
htmlPage content = (X.header << (X.thetitle << "Testing forms"))
  +++ (X.body << content)

register :: XForm Registration
register = Registration <$> user <*> passConfirmed

user :: XForm String
user = pure_user `F.checkM` F.ensureM valid error where
    valid name = return True
    error = "Username already exists in the database!"
 
pure_user :: XForm String
pure_user = input `F.check` F.ensure valid error where
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

main = simpleHTTP (nullConf {port = 9959}) handleRegistration
