module Handler.Admin where

-- TODO: Сделать возможность загрузки CSV-файлов

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Data.Time
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

data LoadFileType =
      RoomsFile FilePath
    | ReportsFile FilePath
    | UnknownFile

data LoadRoomsForm = LoadRoomsForm {
    fileInfo :: FileInfo
}

data ReportFileCommand =
      RemoveReport Text
    | AddReport Text Text TimeOfDay Day Text
    | ReportParsingError


data RoomFileCommand =
      RemoveRoom Text
    | AddRoom Text Int
    | RoomParsingError

uploadDirectory :: FilePath
uploadDirectory = "static"

getAdminR :: Handler Html
getAdminR = do 
    (widget, enctype) <- generateFormPost roomsForm
    defaultLayout
        [whamlet|
            <center>
                <p>
                    Загрузка файла:
                <p> rooms.txt - файл аудиторий
                <p> reports.txt - файл докладов
                <form method=post action=@{AdminR} enctype=#{enctype}>
                    ^{widget}
                    <button>Отправить
        |]

postAdminR :: Handler Html
postAdminR = do 
    ((result, widget), enctype) <- runFormPost roomsForm
    case result of
        FormSuccess file -> do
            fl <- writeToServer $ fileInfo file
            case fl of
                RoomsFile path -> 
                    runDB (addRoomsFromFile path) >>
                    defaultLayout [whamlet|<p>Аудитории загружены.|]
                ReportsFile path -> 
                    runDB (addReportsFromFile path) >>
                    defaultLayout [whamlet|<p>Отчёты загружены.|]
                _ -> defaultLayout [whamlet|<p>Неизвестный файл.|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{AdminR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

parseRoom :: [String] -> RoomFileCommand
parseRoom ["a", rid, seats] = AddRoom (fromString rid) (read seats)
parseRoom ["r", rid, seats] = RemoveRoom (fromString rid)
parseRoom _ = RoomParsingError

addRoomsFromFile path = do
    content <- liftIO $ SIO.readFile $ filePath path
    let rooms =  map (parseRoom . words) $ lines content 
    forM_ rooms $ \room ->
        case room of
            RemoveRoom idnt -> (removeRoom idnt) >> return ()
            AddRoom idnt seats -> do
                rs <- selectFirst [RoomRoomident ==. idnt] []
                case rs of
                    Nothing -> (addRoom idnt seats) >> return ()
                    _ -> return ()
            RoomParsingError -> return ()
    return ()

addReportsFromFile path = do
    content <- liftIO $ SIO.readFile $ filePath path
    let reps = map (parseReport . words) $ lines content
    forM_ reps $ \rep ->
        case rep of
            RemoveReport idnt -> (removeReport idnt) >> return ()
            AddReport title reptr time day rid -> do
                reports <- selectList [ReportTitle ==. title] []
                (addReport title reptr time day rid) >> return ()
            ReportParsingError -> return ()


parseReport ["a", title, reporter, time, day, roomid] = 
    AddReport (fromString title) (fromString reporter) 
              (read time) (read day) (fromString roomid)
parseReport _ = ReportParsingError

chooseFileType filename
    | filename == "rooms.txt" = RoomsFile filename
    | filename ==  "reports.txt" = ReportsFile filename
    | otherwise = UnknownFile

writeToServer :: FileInfo -> Handler LoadFileType --FilePath
writeToServer file = do
    let filename = unpack $ fileName file
        path = filePath filename
    liftIO $ fileMove file path
    return $ chooseFileType filename

filePath :: FilePath -> FilePath
filePath f = uploadDirectory </> f

roomsForm :: Form LoadRoomsForm
roomsForm = do
    renderBootstrap3 BootstrapBasicForm $ LoadRoomsForm 
                                       <$> fileAFormReq "Choose a file"
