{-# LANGUAGE ScopedTypeVariables #-}

module DbUtils where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
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
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)
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

{-
Report
    title Text
    reporter Text
    time Text
    day Text
    room RoomId
    seats Int
    UniqueReport title
ReportInfo
    title ReportId
    info Text
    UniqueReportInfo title
ReportState
    title ReportId
    approved Bool
    UniqueReportState title
-}

---------------------------------------------
-- Функции для работы с БД
---------------------------------------------


roomNotExistMsg = "Room does not exist"
userNotExist    = "User does not exist"
reportNotExist  = "Report does not exist"
noFreeSeats     = "There are no free seats"
userRegistered  = "You have already registered" 
okMsg           = "OK"

checkRequestCode = (== okMsg)

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


reportByTitle title = selectFirst [ReportTitle ==. title] [] 

addNewReport title info reporter time day room seats = do
    -- TODO: обработка ситуации, когда аудитория не найдена
    rme <- selectFirst [RoomRoomident ==. room] [] 
    case rme of
        Just (Entity roomKey _) -> do
            rid <- insert $ Report title reporter time day roomKey seats
            _ <- insert $ ReportInfo rid info
            _ <- insert $ ReportState rid False
            return okMsg
        Nothing -> return roomNotExistMsg


{-
 isApproved :: forall (m :: * -> *).
               MonadIO m =>
               Key Report -> ReaderT SqlBackend m Bool
-}
isApprovedKey k = do
    r <- selectFirst [(ReportStateTitle ==. k),(ReportStateApproved ==. True)] []
    case r of 
        Nothing -> return False
        _ -> return True

isApproved title = do
    -- TODO: обработка ситуации, когда не найден доклад
    Just (Entity k _) <- reportByTitle title
    isApprovedKey k

approve title = do
    Just (Entity kr (Report _ _ _ _ r _)) <- reportByTitle title 
    _ <- updateWhere [ ReportStateTitle ==. kr] [ ReportStateApproved =. True]
    Just (Room _ s) <- get r
    _ <- updateWhere [ ReportTitle ==. title] [ ReportSeats =. s]
    return ()

getReports = do
    dat <- selectList [] []
    return (dat :: [Entity Report])
  
getApprovedReports = do
    reps <- getReports
    filterM (\(Entity key _) -> isApprovedKey key >>= return) reps

getNotApprovedReports = do
    reps <- getReports
    filterM (\(Entity key _) -> isApprovedKey key >>= (return . not)) reps

getRooms :: forall (m :: * -> *).
            MonadIO m =>
            ReaderT SqlBackend m [Entity Room]
getRooms = do
    dat <- selectList [] []
    return (dat :: [Entity Room])

maybeToEither v msg f =
    case v of
        Nothing -> Left msg
        Just x -> f x

-- Добавляет новую аудиторию
addRoom ident seats = do insert $ Room ident seats

addReport title reporter time (day :: Text) roomid = do
    room <- selectFirst [ RoomRoomident ==. roomid ] []

    case room of 
        Nothing -> return roomNotExistMsg
        Just (Entity xid x) -> do
            (insert 
                  $ Report title reporter time day xid (roomMaxseats x))
                  >> return okMsg

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