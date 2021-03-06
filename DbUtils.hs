{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DbUtils where

import Database.Persist.Sqlite              (createSqlitePool, runSqlPool,
                                             sqlDatabase, sqlPoolSize)

import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import Database.Persist.Sql

data FullReport = FullReport {
                    frTitle :: Text,
                    frReporter :: Text,
                    frTime :: Text,
                    frDay :: Text,
                    frRoom :: Text,
                    frSeats :: Int,
                    frInfo :: Text,
                    frApproved :: Bool
}

---------------------------------------------
-- Функции для работы с БД
---------------------------------------------

---------------------------------------------
-- Вспомогательные функции
---------------------------------------------
-- Забирает значение из Entity
dropEntity :: Entity b -> b
dropEntity (Entity _ x) = x

-- Забирает значение из списка Entity
dropEntityList :: [Entity b] -> [b]
dropEntityList = map dropEntity

-- Добавляет сообщение с текущей датой в лог
logm message = do
    time <- liftIO getCurrentTime
    insert $ Log message time

-- Проверяет, что ответ не является ошибкой
checkRequestCode :: forall a. (Eq a, IsString a) => a -> Bool
checkRequestCode = (== okMsg)

-- Возвращает лог
getLog = do
    l <- selectList [] [Desc LogMtime]
    return l

-- Возвращает последние n записей из лога
getLogN n = do
    l <- selectList [] [Desc LogMtime]
    return $ take n l

maybeToEither :: forall t a b.
                 Maybe t -> a -> (t -> Either a b) -> Either a b
maybeToEither v msg f =
    case v of
        Nothing -> Left msg
        Just x -> f x

---------------------------------------------
-- Константы
---------------------------------------------
requestExists = "Запрос с таким именем уже существует"
reportExists  = "Доклад с таким именем уже существует"

roomNotExistMsg :: forall a. IsString a => a
roomNotExistMsg = "Room does not exist"

userNotExist :: forall a. IsString a => a
userNotExist    = "User does not exist"

reportNotExist :: forall a. IsString a => a
reportNotExist  = "Report does not exist"

noFreeSeats :: forall a. IsString a => a
noFreeSeats     = "There are no free seats"

userRegistered :: forall a. IsString a => a
userRegistered  = "You have already registered" 

okMsg :: forall a. IsString a => a
okMsg           = "OK"



---------------------------------------------
-- Функции для работы с запросами
---------------------------------------------

getSiteData = do
    c <- selectFirst [] []
    case c of
        Nothing -> 
            return $ insert $ SiteData "default" 100
        Just (Entity _ d) 
                -> return d

getLogSize = do
    c <- selectFirst [] []
    case c of
        Nothing -> 
            return 100
        Just (Entity _ (SiteData _ logn)) 
            -> return $ logn

setLogSize n = do
    c <- selectFirst [SiteDataConfigName ==. "default"] []
    case c of
        Nothing -> 
            insert $ SiteData "default" n
        Just (Entity k x) -> do
            _ <- update k [SiteDataLogn =. n]
            return k

-- Проверяет, подтверждён доклад или нет 
isApprovedKey :: forall (m :: * -> *).
                  MonadIO m =>
                  -- Ключ доклада в БД
                  Key Report -> 
                      ReaderT SqlBackend m Bool
isApprovedKey k = do
    r <- selectFirst [(ReportStateTitle ==. k),(ReportStateApproved ==. True)] []
    case r of 
        Nothing -> return False
        _ -> return True

-- Проверяет, подтверждён доклад или нет
isApproved :: forall (m :: * -> *).
              MonadIO m =>
              -- Название доклада
              Text ->  
                    ReaderT SqlBackend m Bool
isApproved title = do
    r <- reportByTitle title
    case r of
            Nothing -> return False
            Just (Entity k _) -> isApprovedKey k

approve :: forall (m :: * -> *).
           MonadIO m =>
           Text -> ReaderT SqlBackend m ()
approve title = do
    Just (Entity kr (Report _ _ _ _ r _)) <- reportByTitle title 
    _ <- updateWhere [ ReportStateTitle ==. kr] [ ReportStateApproved =. True]
    Just (Room _ s) <- get r
    _ <- updateWhere [ ReportTitle ==. title] [ ReportSeats =. s]
    return ()

---------------------------------------------
-- Функции для работы с отчётами
---------------------------------------------

addReportRequest title info reporter = do
    req <- selectFirst [ReportRequestTitle ==. title] []
    case req of
        Nothing -> do
            _ <- insert $ ReportRequest title reporter info
            return okMsg
        _       -> 
            do return requestExists

getRequests = do
    r <- selectList [] []
    return $ (r :: [Entity ReportRequest])


-- Возвращает наиболее полные данные о докладе
makeFullReport :: forall (m :: * -> *) a.
                   (IsString a, MonadIO m) =>
                   Text -> ReaderT SqlBackend m (Either a FullReport)
makeFullReport title = do
    rep <- selectFirst [ReportTitle ==. title] []
    case rep of
        Nothing -> return $ Left reportNotExist
        Just (Entity key (Report _ reporter time day room seats)) -> do
            infoData <- selectFirst [ReportInfoTitle ==. key] []
            let info = 
                        case infoData of
                            Nothing -> ""
                            Just (Entity _ (ReportInfo _ i)) -> i
            apprData <- selectFirst [ReportStateTitle ==. key] []
            let appr =
                        case apprData of
                            Nothing -> False
                            Just (Entity _ (ReportState _ a)) -> a
            roomData <- get room
            let roomName = 
                        case roomData of
                            Nothing -> ""
                            Just (Room n _) -> n

            return $ Right $ FullReport title reporter time day roomName seats info appr

approveRequest title time day room = do
    rm  <- selectFirst [RoomRoomident ==. room] []
    req <- selectFirst [ReportRequestTitle ==. title] []
    case rm of 
        Nothing -> do
            _ <- logm (roomNotExistMsg `mappend` " on insert: " `mappend` title) 
            return roomNotExistMsg
        Just (Entity _ (Room rid seats)) ->
            case req of
                Nothing -> do
                    _ <- logm (reportNotExist `mappend` " on insert: " `mappend` title)
                    return reportNotExist
                Just (Entity _ (ReportRequest title reporter info)) -> do
                    _ <- deleteWhere [ReportRequestTitle ==. title]
                    return $ addNewReport title info reporter time day room 
                    

reportByTitle :: forall (m :: * -> *).
                 MonadIO m =>
                 Text -> ReaderT SqlBackend m (Maybe (Entity Report))
reportByTitle title = selectFirst [ReportTitle ==. title] [] 


reportsByReporter rep = selectList [ReportReporter ==. rep] []

updateReport  title info reporter time day room seats = do
    rep <- selectFirst [ReportTitle ==. title] []
    case rep of
        Just (Entity rid _) -> do
            rme <- selectFirst [RoomRoomident ==. room] [] 
            case rme of
                Just (Entity roomKey _) -> do
                    _ <- updateWhere 
                        [ReportTitle ==. title] 
                        [ ReportReporter =. reporter,
                          ReportTime     =. time,
                          ReportDay      =. day,
                          ReportRoom     =. roomKey,
                          ReportSeats    =. seats]
                    _ <- updateWhere [ReportInfoTitle ==. rid]  [ReportInfoInfo =. info]
                    return okMsg
                Nothing -> do
                    _ <- logm (roomNotExistMsg `mappend` " on insert: " `mappend` title)
                    return roomNotExistMsg
        Nothing -> do
            _ <- logm (reportNotExist `mappend` " on insert: " `mappend` title)
            return reportNotExist

-- Добавляет полную информацию о докладе в БД
addNewReport :: forall b (m :: * -> *).
                (IsString b, MonadIO m) =>
                Text    -- Заголовок доклада 
                -> Text -- Информация о докладе
                -> Text -- Автор доклада
                -> Text -- Время доклада
                -> Text -- День доклада
                -> Text -- название аудитории
                -> Int  -- Количество свободных мест
                -> ReaderT SqlBackend m b
addNewReport title info reporter time day room seats = do
    rep <- selectFirst [ReportTitle ==. title] []
    case rep of
        Nothing -> do
            rme <- selectFirst [RoomRoomident ==. room] [] 
            case rme of
                Just (Entity roomKey _) -> do
                    rid <- insert $ Report title reporter time day roomKey seats
                    _ <- insert $ ReportInfo rid info
                    _ <- insert $ ReportState rid False
                    return okMsg
                Nothing -> do
                    _ <- logm (roomNotExistMsg `mappend` " on insert: " `mappend` title)
                    return roomNotExistMsg
        _ -> do
            _ <- logm (reportExists `mappend` " on insert: " `mappend` title) 
            return reportExists

addReport :: forall b (m :: * -> *).
             (IsString b, MonadIO m) =>
             Text -> Text -> Text -> Text -> Text -> ReaderT SqlBackend m b
addReport title reporter time (day :: Text) roomid = do
    room <- selectFirst [ RoomRoomident ==. roomid ] []

    case room of 
        Nothing -> return roomNotExistMsg
        Just (Entity xid x) -> do
            (insert 
                  $ Report title reporter time day xid (roomMaxseats x))
                  >> return okMsg

getReports :: forall (m :: * -> *).
              MonadIO m =>
              ReaderT SqlBackend m [Entity Report]
getReports = do
    dat <- selectList [] []
    return (dat :: [Entity Report])

getApprovedReports :: forall (m :: * -> *).
                       MonadIO m =>
                       ReaderT SqlBackend m [Entity Report]
getApprovedReports = do
    reps <- getReports
    filterM (\(Entity key _) -> isApprovedKey key >>= return) reps

getNotApprovedReports :: forall (m :: * -> *).
                         MonadIO m =>
                         ReaderT SqlBackend m [Entity Report]
getNotApprovedReports = do
    reps <- getReports
    filterM (\(Entity key _) -> isApprovedKey key >>= (return . not)) reps

---------------------------------------------
-- Функции для работы с пользователями
---------------------------------------------

getUsers :: forall (m :: * -> *).
             MonadIO m =>
             ReaderT SqlBackend m [Entity User]
getUsers = do
    usrs <- selectList [] []
    return $ (usrs :: [Entity User])

getUsersInfo = do
    usrs <- selectList [] []
    return (usrs :: [Entity UserInfo])

addUserInfo ident accom = do return $ insert $ UserInfo ident accom
addSponsor name ref = do return $ insert $ Sponsor name ref

-- Возвращает список спонсоров
getSponsors :: forall (m :: * -> *).
               MonadIO m =>
               ReaderT SqlBackend m [Entity Sponsor]
getSponsors = do
    l <- selectList [] []
    return $ (l :: [Entity Sponsor])



---------------------------------------------
-- Прочее 
---------------------------------------------

getRooms :: forall (m :: * -> *).
            MonadIO m =>
            ReaderT SqlBackend m [Entity Room]
getRooms = do
    dat <- selectList [] []
    return (dat :: [Entity Room])

-- Добавляет новую аудиторию
addRoom :: forall (m :: * -> *).
           MonadIO m =>
           Text -> Int -> ReaderT SqlBackend m (Key Room)
addRoom ident seats = do insert $ Room ident seats

removeRoom ident = do
    deleteWhere [RoomRoomident ==. ident]

removeReport ident = do
    deleteWhere [ReportTitle ==. ident]

-- Занимает место на докладе
removeSeat (Entity rpid r) = do
    if (reportSeats r) > 0 
        then do
            _ <- update rpid [ReportSeats =. reportSeats r - 1]
            return okMsg
        else do return noFreeSeats

-- Занимает место для пользователя
-- TODO: убрать case'ы
visitReport :: forall (m :: * -> *) b (m1 :: * -> *).
               (IsString b, IsString (ReaderT SqlBackend m1 b), MonadIO m,
                MonadIO m1) =>
               Text -> Text -> ReaderT SqlBackend m (ReaderT SqlBackend m1 b)
visitReport uid rid = do
    user    <- selectFirst [ UserIdent ==. uid ] []
    report  <- selectFirst [ ReportTitle ==. rid ] []

    case user of
        Nothing ->
            return $ userNotExist
        Just (Entity usid _) -> 
            case report of 
                Nothing -> 
                    return $ reportNotExist
                Just (Entity rpid r) -> do
                    subscr  <- selectFirst  [ SubscriptionsUserid ==. usid, 
                                            SubscriptionsReportid ==. rpid] []
                    return $ case subscr of
                        Nothing -> 
                            removeSeat (Entity rpid r)
                        Just _  -> 
                            userRegistered