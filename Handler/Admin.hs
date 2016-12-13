module Handler.Admin where

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

data LoadRoomsForm = LoadRoomsForm {
    fileInfo :: FileInfo
}

uploadDirectory :: FilePath
uploadDirectory = "static"

getAdminR :: Handler Html
getAdminR = do 
    (widget, enctype) <- generateFormPost roomsForm
    defaultLayout
        [whamlet|
            <p>
                The widget generated contains only the contents
                of the form, not the form tag itself. So...
            <form method=post action=@{AdminR} enctype=#{enctype}>
                ^{widget}
                <p>It also doesn't include the submit button.
                <button>Submit
        |]

postAdminR :: Handler Html
postAdminR = do 
    ((result, widget), enctype) <- runFormPost roomsForm
    case result of
        FormSuccess file -> do
            path <- writeToServer $ fileInfo file
            runDB (addRoomsFromFile path)

            defaultLayout [whamlet|<p>Загружено|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{AdminR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

data RoomFileCommand =
      RemoveRoom Text
    | AddRoom Text Int
    | RoomParsingError

parseRoom :: [String] -> RoomFileCommand
parseRoom ["a", rid, seats] = AddRoom (fromString rid) (read seats)
parseRoom ["r", rid, seats] = RemoveRoom (fromString rid)
parseRoom _ = RoomParsingError

addRoomsFromFile path = do
    content <- liftIO $ SIO.readFile $ filePath path
    let rooms =  map (parseRoom . words) $ lines content 
    forM_ rooms $ \room ->
        case room of
            RemoveRoom idnt -> removeRoom idnt >> return ()
            AddRoom idnt seats -> addRoom idnt seats >> return ()
            RoomParsingError -> return ()
    return ()
        
writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let filename = unpack $ fileName file
        path = filePath filename
    liftIO $ fileMove file path
    return filename

filePath :: FilePath -> FilePath
filePath f = uploadDirectory </> f

roomsForm :: Form LoadRoomsForm
roomsForm = do
    renderBootstrap3 BootstrapBasicForm $ LoadRoomsForm 
                                       <$> fileAFormReq "Choose a file"
