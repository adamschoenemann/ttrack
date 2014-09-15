{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import TimeTrackerDB
import TimeTrackerTypes
import TimeTrackerUtils
import System.Directory (removeFile, doesFileExist)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import Data.Time
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Char (toLower)
import System.Environment
import Network.Socket (withSocketsDo)
import Control.Exception
import Prelude hiding (handle, catch)
import System.Time
import System.Locale
import Data.Char



dbname :: String
dbname = "hstt.db"

syntaxError :: String
syntaxError = "Usage: ttrack command\n\
              \\n\
              \commands:\n\
              \\t create [task]\t\t\t creates a new task\n\
              \\t start [task]\t\t\t starts tracking task\n\
              \\t end \t\t\t\t stops tracking current task\n\
              \\t current \t\t\t get current task in progress\n\
              \\t duration \t\t\t print duration of current session\n\
              \\t list \t\t\t\t lists all tasks\n\
              \\t time [task] \t\t\t print time spent on task \n\
              \\t remove [task] \t\t\t removes task and all sessions for that task"

handleInput :: [String] -> TrackerMonad ()
handleInput args = do
    case args of
        ["create", n] -> do
            create n
            return ()
        ["start", n] -> do
            start n
            return ()
        ["current"] -> do
            task <- current
            tell ["Current task is " ++ taskName task]
            return ()
        ["end"] -> do
            sess <- end
            tell ["Session duration was " ++ (readSeconds . round . fromJust $ sessDuration sess)]
            return ()
        ["list"] -> do
            list
            return ()
        ["duration"] -> do
            d <- duration
            task <- current
            tell ["Current session is with task: " ++ taskName task
                 ++ ". Session duration: " ++ (readSeconds $ round d)]
            return ()
        ["remove", n] -> do
            remove n
            return ()
        ["time", n] -> do
            t <- time n
            tell $ ["Time spent on task " ++ n ++ " is " ++ (readSeconds $ round t)]
            return ()
        ["time", n, from, to] -> do
            -- TODO: Implement this using parseISO
            -- It should print time spent on task in the time-period given by the interval
            -- [from ; to]
            -- E.g. make a function in TimeTrackerDB that retrieves sessions within a time period
            -- and adds them up
            let froms = parseISO from
            case froms of
                Left x -> throwError x
                Right froms' -> do
                    let tos = parseISO to
                    case tos of
                        Left x -> throwError x
                        Right tos' -> do
                            t <- getTaskByName n
                            ss <- getTaskSessionsInInterval t (dateString froms') (dateString tos')
                            let time = round . getSum . mconcat $
                                            map (Sum . fromJust . sessDuration) ss
                            tell ["Time spent on task: " ++ n ++ ": " ++ readSeconds time]
                            return ()
                where dateString t = let tc = defaultTimeLocale
                                     in  formatTime tc "%F %T" t
        _ -> do
            tell [syntaxError]
            return ()


main :: IO ()
main = withSocketsDo $ handle errorHandler $ bracket acquire finalize proc
    where
        acquire = do
            dbh <- connect dbname
            return dbh

        proc dbh = do withTransaction dbh doSql
            where doSql dbh = do
                    args <- getArgs
                    r <- runTrackerMonad (handleInput args) dbh
                    case r of
                        Left err -> do
                            putStrLn $ unwrapTTError err
                            rollback dbh
                        Right (v,msg) -> mapM_ putStrLn msg

        finalize dbh = do disconnect dbh
        errorHandler :: SomeException -> IO ()
        errorHandler e = putStrLn $ "An error occured: " ++ show e





create :: String -> TrackerMonad Task
create name = do
    last <- getLastSession
    if isEnded last
        then do
            task <- createTask name
            tell $ ["Created task: " ++ name]
            return task
        else do
            throwError $ OtherSessionStarted $ "A session is already in progress. Please close it \
                                                \before creating a new task."

start :: String -> TrackerMonad Session
start name = do
    t <- getTaskByName name
    last <- getLastSession
    if not (isEnded last)
        then throwError $ OtherSessionStarted $
                "Can't start a new session when session " ++ (taskName . sessTask $ last)
                ++ " is already open. Please close it first"
        else do
            s <- startSession t
            tell ["Started tracking " ++ name]
            return s
    `catchError` errorHandler
        where   errorHandler (NoTaskFound msg) = do
                    liftIO $ putStrLn $ "No task with name " ++ name ++ " was found. Create new task? (y/n)"
                    resp <- liftIO $ getLine
                    if (map toLower resp == "y")
                        then do create name
                                start name
                        else do throwError $ OtherError $ "No action taken"
                errorHandler e = throwError e




current :: TrackerMonad Task
current = do
    last <- getLastSession
    if isEnded last
        then throwError $ NoTaskFound $ "No current task could be found"
        else do
            let task = sessTask last
            return $ task



end :: TrackerMonad Session
end = do
    lastSess <- getLastSession
    endSess <- endSession lastSess
    let (Just dur) = sessDuration endSess
    tellUsr $ "Session duration was " ++ renderDuration dur ++ ".\nIs this correct? (y/n)"
    resp <- liftIO getLine
    if isCorrect resp
        then do
            tell ["Ended task " ++ (show . taskName . sessTask) lastSess]
            return endSess
        else inputDuration endSess

    where   isCorrect r
                     | (map toUpper r) == "Y" = True
                     | otherwise = False
            inputDuration sess = do
                tellUsr "Please input duration in format [hm]s e.g. 1h30m10s"
                durString <- liftIO getLine
                let durDiffTime = parseDuration durString
                case durDiffTime of
                    Just d -> do
                        newSess <- setSessDuration sess d
                        return newSess
                    Nothing -> do
                        liftIO $ putStrLn "Incorrect duration format. Please use hh:mm (e.g. 02:33)"
                        inputDuration sess
            tellUsr = liftIO . putStrLn




list :: TrackerMonad [Task]
list = do
    tasks <- getTasks
    tell ["Listing tasks..."]
    mapM_ (\t -> tell ['\t':taskName t]) tasks
    return tasks


remove :: String -> TrackerMonad (Task, [Session])
remove name = do
    task <- removeTask name
    tell ["Removing task " ++ name]
    sessions <- removeTaskSessions task
    tell ["Removing " ++ show (length sessions) ++ " session(s)"]
    return (task, sessions)


time :: String -> TrackerMonad NominalDiffTime
time n = do
    task <- getTaskByName n
    sessions <- getTaskSessions task
    case sessions of
        [] -> return (0 :: NominalDiffTime)
        s -> do
            -- TODO. This will fail when there is more than one un-ended session or
            -- if the un-ended session is not the last
            durs <- liftIO $ mapM (sessDurationIO) s
            let time = foldr (\x acc -> x+acc) 0 durs
            return time

duration :: TrackerMonad NominalDiffTime
duration = do
    sess <- getLastSession
    if isEnded sess
        then throwError $ NoCurrentSession "No session is currently in progress"
        else do
            dur <- liftIO $ sessDurationIO sess
            return dur

reset = do
    f <- doesFileExist dbname
    when (f) $
        removeFile dbname


showDB = do
    dbh <- connect dbname
    tasks <- quickQuery' dbh "SELECT * FROM tasks" []
    sessions <- quickQuery' dbh "SELECT * FROM sessions" []

    putStrLn $ show tasks
    putStrLn $ show sessions