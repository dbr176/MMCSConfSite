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

---------------------------------------------
-- Типы Данных
---------------------------------------------

data LoadFileType =
      RoomsFile FilePath
    | ReportsFile FilePath
    | UnknownFile

data ApproveRequestFF = ApproveRequestFF {
      arTitle :: Text,
      arTime :: Text,
      arDay :: Text,
      arRoom ::  Text
}

data LoadRoomsForm = LoadRoomsForm {
    fileInfo :: FileInfo
}

data ReportFileCommand =
      RemoveReport Text
    | ApproveRequest Text Text Text Text
    | ApproveReport Text
    | AddReport Text Text Text Text Text
    | UpdateReport Text Text Text Text Text
    | ReportParsingError


data RoomFileCommand =
      RemoveRoom Text
    | AddRoom Text Int
    | RoomParsingError

---------------------------------------------
-- Константы
---------------------------------------------

uploadDirectory :: FilePath
uploadDirectory = "static"

---------------------------------------------
-- Admin
---------------------------------------------

getAdminR :: Handler Html
getAdminR = do
    (widget, enctype) <- generateFormPost roomsForm
    (appWidget, appEnctype) <- generateFormPost approveRequestForm
    rooms <- (runDB $ getRooms)
    reports <- (runDB $ getNotApprovedReports)
    requests <- (runDB $ getRequests)
    log <- runDB getLog
    let rs = dropEntityList rooms
    let rq = dropEntityList requests
    let lg = dropEntityList log 
    defaultLayout [whamlet|<div .container><center>
        
            <p>
                Загрузка файла:
            <p> rooms.txt - файл аудиторий
            <p> reports.txt - файл докладов
            <form method=post action=@{AdminR} enctype=#{enctype}>
                ^{widget}
                <button>Отправить

        <h2> Аудитории
        <table  border="2" bordercolor="black" width="80%" cellpadding="10" cellspacing="40" bgcolor="#0000FF">
            <thead>
                <tr>
                <th bgcolor="#FFFF00">Аудитория</th>
                <th bgcolor="#FFFF00">Всего мест</th>
            <tbody>
                $forall (Room idn sts) <- rs
                    <tr>
                        <td>#{idn}</td>
                        <td>#{show sts}</td>

        <h2> Запросы
        <table border="2" bordercolor="black" width="80%" cellpadding="10" cellspacing="40" bgcolor="#0000FF">
            <thead>
                <tr>
                <th bgcolor="#FFFF00">Название</th>
                <th bgcolor="#FFFF00">Докладчик</th>
            <tbody>
                $forall (ReportRequest title reporter _) <- dropEntityList requests
                     <tr>
                        <td>#{title}</td>
                        <td>#{reporter}</td>

        <form method=post action=@{ApproveFR} enctype=#{appEnctype}>
            ^{appWidget}
            <button>Отправить

        <h2> Неподтверждённые доклады
        <table border="2" bordercolor="black" width="80%" cellpadding="10" cellspacing="40" bgcolor="#0000FF">
            <thead>
                <tr>
                <th bgcolor="#FFFF00">Название</th>
                <th bgcolor="#FFFF00">Докладчик</th>
                <th bgcolor="#FFFF00"></th>
            <tbody>
                $forall (Report title reporter _ _ _ _) <- dropEntityList reports
                     <tr>
                        <td>#{title}</td>
                        <td>#{reporter}</td>
                        <td><a href="/approve/#{title}">Подтвердить

        <h2> Лог
        <table border="2" bordercolor="black" width="80%" cellpadding="10" cellspacing="40" bgcolor="#0000FF">
            <thead>
                <tr>
                <th bgcolor="#FFFF00">Сообщение</th>
                <th bgcolor="#FFFF00">Дата</th>
            <tbody>
                $forall (Log msg time) <- lg
                     <tr>
                        <td>#{msg}</td>
                        <td>#{show time}</td>
    |]

postAdminR :: Handler Html
postAdminR = do
    ((result, widget), enctype) <- runFormPost roomsForm
    case result of
        FormSuccess file -> do
            fl <- writeToServer $ fileInfo file
            case fl of
                RoomsFile path ->
                    runDB (logm "Загрузка аудиторий") >>
                    runDB (addRoomsFromFile path) >>
                    runDB (logm "Аудитории загружены") >>
                    defaultLayout [whamlet|<p>Аудитории загружены. <a href="/admin">Вернуться|]
                ReportsFile path ->
                    runDB (logm "Загрузка докладов") >>
                    runDB (addReportsFromFile path) >>
                    runDB (logm "Доклады загружены") >>
                    defaultLayout [whamlet|<p>Отчёты загружены. <a href="/admin">Вернуться |]
                _ -> defaultLayout [whamlet|<p>Неизвестный файл. <a href="/admin">Вернуться|]
        _ -> defaultLayout
            [whamlet|
                <p>Ошибка, попробуйте снова.
                <div class="form">
                    <form method=post action=@{AdminR} enctype=#{enctype}>
                        ^{widget}
                        <button>Отправить
            |]

---------------------------------------------
-- Approve
---------------------------------------------

appr title time day room = do
    reports <- selectList [ReportTitle ==. title] []
    (Just (Entity _ (ReportRequest _ rep _))) <- selectFirst [ReportRequestTitle ==. title] []
    deleteWhere [ReportRequestTitle ==. title]
    _ <- approve title
    -- Найти комнату
    (addNewReport title ""  rep time day room 0)

postApproveFR :: Handler Html
postApproveFR = do
    ((result, widget), enctype) <- runFormPost approveRequestForm
    case result of
        FormSuccess appr -> do
            _ <- runDB $ logm "Подтверждение запросов"
            let title = arTitle appr
                time  = arTime appr
                day   = arDay appr
                room  = arRoom appr
            --_ <- runDB $ appr title time day room
            reports <- runDB $ selectList [ReportTitle ==. title] [] 
            reports <- runDB $ selectList [ReportTitle ==. title] []
            runDB $ deleteWhere [ReportRequestTitle ==. title]
            (Just (Entity _ (ReportRequest _ rep _))) <- runDB $ selectFirst [ReportRequestTitle ==. title] []
            runDB $ deleteWhere [ReportRequestTitle ==. title]
            runDB $ (addNewReport title ""  rep time day room 0)
            runDB $ logm "Запросы подтверждены"
            defaultLayout [whamlet|<p>Подтверждено. <a href="/admin">Вернуться|]
        _ -> do
            defaultLayout [whamlet|<p>Не подтверждено. <a href="/admin">Вернуться|]

---------------------------------------------
-- Вспомогательные функции
---------------------------------------------

getApproveFR = getAdminR

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
                (addNewReport title "" reptr time day rid 0) >> return ()
            UpdateReport title reptr time day rid -> do
                (updateReport title "" reptr time day rid 0) >> return ()
            ApproveReport title -> approve title >> return ()
            ApproveRequest title time day room -> do
                -- Найти комнату
                reports <- selectList [ReportTitle ==. title] []
                req <- selectFirst [ReportRequestTitle ==. title] []
                case req of
                    (Just (Entity _ (ReportRequest _ rep _))) -> do
                        deleteWhere [ReportRequestTitle ==. title]
                        approve title
                        (addNewReport title ""  rep time day room 0) >> return ()
                    Nothing -> return ()
            ReportParsingError -> return ()


parseReport :: [String] -> ReportFileCommand
parseReport ["a", title, reporter, time, day, roomid] =
    AddReport (fromString title) (fromString reporter)
              (fromString time) (fromString day) (fromString roomid)
parseReport ["p", title] = ApproveReport (fromString title)
parseReport ["r", title] = RemoveReport (fromString title)
parseReport ["u", title, reporter, time, day, roomid] =
    UpdateReport (fromString title) (fromString reporter)
              (fromString time) (fromString day) (fromString roomid)
parseReport ["f", title, t, day, room] = ApproveRequest (fromString title) (fromString t) (fromString day) (fromString room)
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

---------------------------------------------
-- Формы
---------------------------------------------

roomsForm :: Form LoadRoomsForm
roomsForm = do
    renderBootstrap3 BootstrapBasicForm $ LoadRoomsForm
                                       <$> fileAFormReq "Выберите файл"

approveRequestForm :: Form ApproveRequestFF
approveRequestForm = do
    renderBootstrap3 BootstrapBasicForm $ ApproveRequestFF
                                       <$> areq textField "Название" Nothing
                                       <*> areq textField "Время" (Just "")
                                       <*> areq textField "День" (Just "")
                                       <*> areq textField "Аудитория" Nothing

