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
import Text.Julius (RawJS (..))

data RegInform = RegInform {
    name :: Text,
    email :: Text,
    username :: Text,
    phone :: Text
} deriving(Show)

-- TODO: Вставить данные в новую форму
-- <div>
--     <label for="form_sname">Фамилия</label>
--     <input type="Фамилия" id="form_sname" name="second_name"/>
-- <div>
--     <label for="form_fname">Имя</label>
--     <input type="Имя" id="form_fname" name="first_fname"/>
-- <div>
--     <label for="form_tname">Отчество</label>
--     <input type="Отчество" id="form_tname" name="third_name"/>
-- <div>
--     <label for="form_titrep">Название доклада</label>
--     <input type="Название доклада" id="form_titrep" name="title_report"/>
-- <div>
--     <label for="form_about_you">О себе</label>
--     <textarea type="О себе" id="form_about_you" name="about_you"/>
-- <div>
--   <a .btn.btn-info.btn-lg>
--      Подать заявку
getRegistrationR :: Handler Html
getRegistrationR = do defaultLayout [whamlet|<div .container><form action=@{RegistrationR}>
        <div class="form"><form id="contactform">
            <p class="contact"><label for="sname">Фамилия</label></p>
            <input id="sname" name="second_name" placeholder="Введите фамилию" required="" tabindex="1" type="text">

            <p class="contact"><label for="fname">Имя</label></p>
            <input id="fname" name="first_name" placeholder="Введите имя" required="" tabindex="1" type="text">

            <p class="contact"><label for="tname">Отчество</label></p>
            <input id="tname" name="third_name" placeholder="Введите отчество" required="" type="email">

            <p class="contact"><label for="username">Название доклада</label></p>
            <input id="username" name="username" placeholder="Введите тему доклада" required="" tabindex="2" type="text">

            <p class="contact"><label for="phone">О себе</label></p>
            <textarea id="phone" name="phone" placeholder="Напишите о себе что-нибудь интересное" required="" type="text">

            <input class="buttom" name="submit" id="submit" tabindex="5" value="Sign me up!" type="submit">
    |]

getInputR = do
    inf <- runInputGet $ RegInform
                <$> ireq textField "name"
                <*> ireq textField "email"
                <*> ireq textField "username"
                <*> ireq textField "phone"
    addReportRequest (name $ inf) (email $ inf) (username $ inf)

postRegistrationR :: Handler Html
postRegistrationR = undefined
