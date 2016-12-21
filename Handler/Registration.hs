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
    firstName :: Text,
    secondName :: Text,
    thirdName :: Text,
    report :: Text
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
            <p class="contact"><label for="name">Name</label></p>
            <input id="name" name="name" placeholder="First and last name" required="" tabindex="1" type="text">
            <p class="contact"><label for="email">Email</label></p>
            <input id="email" name="email" placeholder="example@domain.com" required="" type="email">
            <p class="contact"><label for="username">Create a username</label></p>
            <input id="username" name="username" placeholder="username" required="" tabindex="2" type="text">
            <p class="contact"><label for="phone">Mobile phone</label></p>
            <input id="phone" name="phone" placeholder="phone number" required="" type="text"> <br>
            <input class="buttom" name="submit" id="submit" tabindex="5" value="Sign me up!" type="submit">
    |]

getInputR :: Handler Html
getInputR = do
    inf <- runInputGet $ RegInform
                <$> ireq textField "first_fname"
                <*> ireq textField "second_name"
                <*> ireq textField "third_name"
                <*> ireq textField "title_report"
    defaultLayout [whamlet|<p>#{show inf}|]

postRegistrationR :: Handler Html
postRegistrationR = undefined
