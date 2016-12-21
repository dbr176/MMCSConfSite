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
} deriving (Show)

getRegistrationR :: Handler Html
getRegistrationR = do 
	defaultLayout 
		[whamlet|
        <form action=@{RegistrationR}>
        <div .container>
                            <div .bs-docs-section>
                                <div .row>
                                    <div .col-lg-12>
                                        
                                           
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

getInputR :: Handler Html
getInputR = do
    inf <- runInputGet $ RegInform
                <$> ireq textField "second_name"
                <*> ireq textField "first_fname"
                <*> ireq textField "third_name"
                <*> ireq textField "title_report"
    defaultLayout [whamlet|<p>#{show inf}|]

postRegistrationR :: Handler Html 
postRegistrationR = undefined