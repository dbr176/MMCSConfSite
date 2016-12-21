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
    firstName  :: Text,
    secondName :: Text,
    thirdName  :: Text,
    report     :: Text,
    about      :: Textarea,
    apartments :: Bool,
    photo      :: FileInfo
}

getRegistrationR :: Handler Html
getRegistrationR = do
	(appWidget, appEnctype) <- generateFormPost inputRegInform

	defaultLayout [whamlet|<div .container>
        <div class="form"><form id="contactform">
            <form method=post action=@{RegistrationR} enctype=#{appEnctype}>
            ^{appWidget}
            <input class="buttom" name="submit" id="submit" tabindex="5" value="Зарегистрироваться" type="submit">
    |]

    --addReportRequest (report $ inf)
    	--(secondName inf ++ firstName inf ++ thirdName inf) (about $ inf)

postRegistrationR :: Handler Html
postRegistrationR = do
	((result, widget), enctype) <- runFormPost inputRegInform
	case result of
		FormSuccess inf -> do
			let reporter   = secondName inf ++ firstName inf ++ thirdName inf
			let	title      = report inf
			let	aboutYou   = about inf
			let apartment  = apartments inf
			let picture    = photo inf
			defaultLayout [whamlet|<p>Подтверждено. <a href="/admin">Вернуться|]
			undefined
		_ -> do
			defaultLayout [whamlet|<p>Произошла ошибка при регистрации. <a href="/registration">Вернуться|]

inputRegInform :: Form RegInform
inputRegInform = do
    renderBootstrap3 BootstrapBasicForm $ RegInform
                <$> areq textField "Имя" Nothing
                <*> areq textField "Фамилия" Nothing
                <*> areq textField "Отчество" Nothing
                <*> areq textField "Название доклада" Nothing
                <*> areq textareaField "О себе" Nothing
                <*> areq checkBoxField "Необходимость в предоставлении жилья" Nothing
                <*> areq fileField "Загрузить фотографию" Nothing
