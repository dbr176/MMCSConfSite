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
getContactsR = do defaultLayout [whamlet|<div .container>
                            <div .bs-docs-section>
                                <div .row>
                                    <div .col-lg-12>
                                        <div .page-header>
                                            <h1 #start>
                                            <p>

                                            <h2>Место провдения:</h2>
                                            344090, Россия, Ростовская область, г. Ростов-на-Дону, ул. Мильчакова 8а
                                            <h2>Телефон:</h2>
                                            +7 (863) 2975 111
                                             <h2  >Факс:</h2>
                                            +7 (863) 2975 113
                                            <h2>Деканат:</h2>
                                            кабинет 111
                                            <h2>Место на карте:</h2>
                                            <iframe src="https://api-maps.yandex.ru/frame/v1/-/CZH8b6Ny" width="660" height="500" frameborder="0"></iframe>
                                    |]


