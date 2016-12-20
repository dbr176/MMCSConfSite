module Handler.Registration where

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getRegistrationR :: Handler Html
getRegistrationR = do defaultLayout [whamlet|<h1>Page of registration|]

postRegistrationR :: Handler Html 
postRegistrationR = do defaultLayout [whamlet|<h1>Page of registration|]