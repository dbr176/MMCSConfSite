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

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home
import Handler.Admin
import Handler.Comment
import Handler.Profile

---------------------------------------------
-- Функции для работы с БД
---------------------------------------------

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
    time TimeOfDay
    day Day
    room RoomId
    seats Int
    UniqueReport title
Subscriptions
    userid UserId
    reportid ReportId
-}

maybeToEither v msg f =
    case v of
        Nothing -> Left msg
        Just x -> f x

-- Добавляет новую аудиторию
addRoom ident seats = do insert $ Room ident seats
addReport title reporter time day roomid = do
    room <- selectFirst [ RoomRoomident ==. roomid ] []
    return $ maybeToEither room
        roomNotExistMsg $
        \(Entity xid x) -> do
            Right $ insert 
                  $ Report title reporter time day xid (roomMaxseats x)

-- Занимает место на докладе
removeSeat (Entity rpid r) = do
    if (reportSeats r) > 0 
        then do
            return $
                Right $ update rpid [ReportSeats =. reportSeats r - 1]
        else do return $ Left noFreeSeats

-- Занимает место для пользователя
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