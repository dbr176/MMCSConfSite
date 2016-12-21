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
    report :: Text,
    about :: Text
} deriving(Show)

getRegistrationR :: Handler Html
getRegistrationR = do defaultLayout [whamlet|<div .container><form action=@{RegistrationR}>
        <div class="form"><form id="contactform">
            <p class="contact"><label for="sname">Фамилия</label></p>
            <input id="sname" name="second_name" placeholder="Введите фамилию" required="" tabindex="1" type="text">

            <p class="contact"><label for="fname">Имя</label></p>
            <input id="fname" name="first_name" placeholder="Введите имя" required="" tabindex="2" type="text">

            <p class="contact"><label for="tname">Отчество</label></p>
            <input id="tname" name="third_name" placeholder="Введите отчество" required="" tabindex="3" type="text">

            <p class="contact"><label for="treport">Название доклада</label></p>
            <input id="treport" name="title_report" placeholder="Введите тему доклада" required="" tabindex="4" type="text">

            <p class="contact"><label for="_about_you">О себе</label></p>
            <textarea id="_about_you" name="about_you" placeholder="Напишите о себе что-нибудь интересное" tabindex="5" type="textarea">
            <center><input class="buttom" name="submit" id="submit" tabindex="6" value="Зарегистрироваться" type="submit">
    |]

    --addReportRequest (report $ inf)
    	--(secondName inf ++ firstName inf ++ thirdName inf) (about $ inf)

postRegistrationR :: Handler Html
postRegistrationR = undefined

getInputR :: Form RegInform
getInputR = do
    renderBootstrap3 BootstrapBasicForm $ RegInform
                <$> areq textField "first_name" Nothing
                <*> areq textField "second_name" Nothing
                <*> areq textField "third_name" Nothing
                <*> areq textField "title_report" Nothing
                <*> areq textField "about_you" Nothing
