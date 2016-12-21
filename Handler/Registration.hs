module Handler.Registration where

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Data.Time
import Data.List.Split
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Database.Persist.Sql
import Database.Persist

data RegInform = RegInform {
    firstName :: Text,
    secondName :: Text,
    thirdName :: Text,
    report :: Text
}


getRegistrationR :: Handler Html
getRegistrationR = do defaultLayout [whamlet|<div .container>
        <h1> Регистрация </h1>
        <h4>
           <div>
                <label for="form_sname">Фамилия</label>
                <input type="Фамилия" id="form_sname" name="second_name"/>
            <div>
                <label for="form_fname">Имя</label>
                <input type="Имя" id="form_fname" name="first_fname"/>
            <div>
                <label for="form_tname">Отчество</label>
                <input type="Отчество" id="form_tname" name="third_name"/>
            <div>
                <label for="form_titrep">Название доклада</label>
                <input type="Название доклада" id="form_titrep" name="title_report"/>
           <div>
              <a .btn.btn-info.btn-lg>
                 Подать заявку
        |]

postRegistrationR :: Handler Html
postRegistrationR = do defaultLayout [whamlet|<h1>Page of registration|]
