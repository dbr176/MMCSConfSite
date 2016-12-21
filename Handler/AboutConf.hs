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
        <h2  >Описание:</h2>
        Для участинков конференции предоставляются общежития, вкусные блюда.
        <h2  >Время проведения:</h2>
        когда-то
        <h2>Карта проезда:</h2>
        <iframe src="https://api-maps.yandex.ru/frame/v1/-/CZH8b6Ny" width="560" height="400" frameborder="0"></iframe>
        <h2>Фотографии:</h2>
        <p><img src="static/mech.jpg" width="80%" height="50%" alt="фасад мехмата"></p>
    |]



