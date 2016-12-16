module Handler.AboutConf where

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Control.Monad
import Database.Persist.Sql

getAboutConfR :: Handler Html
getAboutConfR = do  defaultLayout [whamlet|<div .container>
                            <div .bs-docs-section>
                                <div .row>
                                    <div .col-lg-12>
                                        <div .page-header>
                                            <h1 #start>
                                            <p>

                                           <h2  >Описание:</h2>
                                            .....
                                            <h2  >Время проведения:</h2>
                                            ...
                                            <h2>Карта проезда:</h2>
                                            <iframe src="https://api-maps.yandex.ru/frame/v1/-/CZHr643B" width="660" height="480" frameborder="0"></iframe>
                                            <h2>Фотографии:</h2>
                                    |]


    
