module Handler.Admin where

import Import
--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
--import Text.Julius (RawJS (..))

getAdminR :: Handler Html
getAdminR = do defaultLayout [whamlet|<h1>Hello World!|]

postAdminR :: Handler Html
postAdminR = do defaultLayout [whamlet|<h1>Hello World!|]
