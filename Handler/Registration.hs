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
import System.Directory

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

    defaultLayout [whamlet|<div .container><center>
    <div class="form"><form id="contactform"
            <form method=post action=@{RegistrationR} enctype=#{appEnctype}>
                ^{appWidget}
                <input class="buttom" name="submit" id="submit" tabindex="6" value="Зарегистрироваться" type="submit">
    |]

postRegistrationR :: Handler Html
postRegistrationR = do
    ((result, widget), enctype) <- runFormPost inputRegInform
    case result of
        FormSuccess inf -> do
            let reporter   = secondName inf ++ ("_"::Text) ++ firstName inf ++("_"::Text) ++ thirdName inf
            let title      = report inf
            let aboutYou   = about inf
            let apartment  = apartments inf
            let picture    = photo inf
            filename <- writeToServer picture
            curDir <- liftIO getCurrentDirectory
            let curFile = curDir ++ "\\static\\" ++ filename
            let newFile = curDir ++ "\\static\\members\\" ++ unpack(reporter) ++ ".jpg"
            liftIO $ copyFile curFile newFile
            liftIO $ removeFile curFile
            uid <- runDB $ insert $ (User reporter (Just (unTextarea aboutYou)))
            runDB $ insert $ (UserInfo uid (
                if apartment == True then
                    Just "Необходимо жилье"
                else
                    Nothing)
                )
            runDB $ insert $ (ReportRequest title reporter (unTextarea aboutYou))
            defaultLayout [whamlet|<p>Регистрация прошла успешно. <a href="/registration">Вернуться|]
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

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let filename = unpack $ fileName file
        path = imageFilePath filename
    liftIO $ fileMove file path
    return filename

uploadDirectory :: FilePath
uploadDirectory = "static"

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f
