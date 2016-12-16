module Handler.Contacts where

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Control.Monad
import Database.Persist.Sql

getContactsR :: Handler Html
getContactsR = do  defaultLayout [whamlet|<h1>Hello World!|]

    
