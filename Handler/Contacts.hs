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
getContactsR = do  defaultLayout [whamlet|<div .container>
                            <div .bs-docs-section>
                                <div .row>
                                    <div .col-lg-12>
                                        <div .page-header>
                                            <h1 #start>
                                            <p>

                                           <h2  >Место провдения:</h2>
                                            ул. Мильчакова 8а и всякое такое!
                                        <h2>Место на карте:</h2>
                                            <iframe src="https://api-maps.yandex.ru/frame/v1/-/CZHr643B" width="560" height="400" frameborder="0"></iframe>
                                                
                                    |]

    
