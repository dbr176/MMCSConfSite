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

---------------------------------------------
-- Функции для работы с БД
---------------------------------------------

--instance PathPiece (Key Report) where
    --toPathPiece (Natural i) = T.pack $ show i
    --fromPathPiece s = 

roomNotExistMsg = "Room does not exist"
userNotExist    = "User does not exist"
reportNotExist  = "Report does not exist"
noFreeSeats     = "There are no free seats"
userRegistered  = "You have already registered" 

{-
Room
    roomident Text
    maxseats Int
    UniqueRoom roomident
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
Subscriptions
    userid UserId
    reportid ReportId
-}

reportByTitle title = selectFirst [ReportTitle ==. title] [] 

addNewReport title info reporter time day room seats = do
    -- TODO: обработка ситуации, когда аудитория не найдена
    Just (Entity roomKey _) <- selectFirst [RoomRoomident ==. room] [] 
    rid <- insert $ Report title reporter time day roomKey seats
    _ <- insert $ ReportInfo rid info
    _ <- insert $ ReportState rid False
    return rid


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
        Nothing -> return ()
        Just (Entity xid x) -> do
            (insert 
                  $ Report title reporter time day xid (roomMaxseats x))
                  >> return ()

    --return $ maybeToEither room
    --    roomNotExistMsg $
    --    \(Entity xid x) -> do
    --        Right $ insert 
    --              $ Report title reporter time day xid (roomMaxseats x)

removeRoom ident = do
    deleteWhere [RoomRoomident ==. ident]

removeReport ident = do
    deleteWhere [ReportTitle ==. ident]

-- Занимает место на докладе
removeSeat (Entity rpid r) = do
    if (reportSeats r) > 0 
        then do
            return $
                Right $ update rpid [ReportSeats =. reportSeats r - 1]
        else do return $ Left noFreeSeats

-- Занимает место для пользователя
-- TODO: убрать case'ы
visitReport uid rid = do
    user    <- selectFirst [ UserIdent ==. uid ] []
    report  <- selectFirst [ ReportTitle ==. rid ] []

    case user of
        Nothing ->
            return $ Left userNotExist
        Just (Entity usid _) -> 
            case report of 
                Nothing -> 
                    return $ Left reportNotExist
                Just (Entity rpid r) -> do
                    subscr  <- selectFirst  [ SubscriptionsUserid ==. usid, 
                                            SubscriptionsReportid ==. rpid] []
                    return $ case subscr of
                        Nothing -> 
                            removeSeat (Entity rpid r)
                        Just _  -> 
                            Left userRegistered