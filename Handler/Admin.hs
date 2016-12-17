module Handler.Admin where

-- TODO: Сделать возможность загрузки CSV-файлов

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Data.Time
import Data.List.Split
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
--import Text.Julius (RawJS (..))
import Database.Persist.Sql
import Database.Persist

data LoadFileType =
      RoomsFile FilePath
    | ReportsFile FilePath
    | UnknownFile

data LoadRoomsForm = LoadRoomsForm {
    fileInfo :: FileInfo
}

data ReportFileCommand =
      RemoveReport Text
    | AddReport Text Text Text Text Text
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
    rooms <- (runDB $ getRooms)
    reports <- (runDB $ getNotApprovedReports)
    let rs =  fmap (\(Entity _ x) -> (roomRoomident x, roomMaxseats x)) rooms
    
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
                <table>
                    <thead>
                        <tr>
                        <th>Аудитория</th>
                        <th>Всего мест</th>
                    <tbody>
                        $forall (idn, sts) <- rs
                            <tr>
                                <td>#{idn}</td>
                                <td>#{show sts}</td>
                <table>
                    <thead>
                        <tr>
                        <th>Название</th>
                        <th>Докладчик</th>
                    <tbody>
                        $forall (Entity _ (Report title reporter _ _ _ _)) <- reports
                             <tr>
                                <td>#{title}</td>
                                <td>#{reporter}</td>
                                <td><a href="/approve/#{title}">Подтвердить 
                        
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
                    defaultLayout [whamlet|<p>Аудитории загружены. <a href="/admin">Вернуться|]
                ReportsFile path -> 
                    runDB (addReportsFromFile path) >>
                    defaultLayout [whamlet|<p>Отчёты загружены. <a href="/admin">Вернуться |]
                _ -> defaultLayout [whamlet|<p>Неизвестный файл. <a href="/admin">Вернуться|]
        _ -> defaultLayout
            [whamlet|
                <p>Ошибка, попробуйте снова.
                <form method=post action=@{AdminR} enctype=#{enctype}>
                    ^{widget}
                    <button>Отправить
            |]

getApproveR :: Text -> Handler Html
getApproveR title = do
    _ <- runDB $ approve title
    getAdminR

parseRoom :: [String] -> RoomFileCommand
parseRoom ["a", rid, seats] = AddRoom (fromString rid) (read seats)
parseRoom ["r", rid, seats] = RemoveRoom (fromString rid)
parseRoom _ = RoomParsingError

addRoomsFromFile path = do
    content <- liftIO $ SIO.readFile $ filePath path
    let rooms =  map (parseRoom . (splitOn ";")) $ lines content 
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
    let reps = map (parseReport . (splitOn ";")) $ lines content
    forM_ reps $ \rep ->
        case rep of
            RemoveReport idnt -> (removeReport idnt) >> return ()
            AddReport title reptr time day rid -> do
                reports <- selectList [ReportTitle ==. title] []
                (addNewReport title "" reptr time day rid 0) >> return ()
            ReportParsingError -> return ()


parseReport :: [String] -> ReportFileCommand
parseReport ["a", title, reporter, time, day, roomid] = 
    AddReport (fromString title) (fromString reporter) 
              (fromString time) (fromString day) (fromString roomid)
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
                                       <$> fileAFormReq "Выберите файл"
