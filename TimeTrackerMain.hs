{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import TimeTrackerDB
import TimeTrackerTypes
import System.Directory (removeFile, doesFileExist)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import Data.Time
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Char (toLower)
import System.Environment
import Network.Socket (withSocketsDo)
import Control.Exception
import Prelude hiding (handle, catch)
import System.Time.Utils



dbname :: String
dbname = "hstt.db"

syntaxError :: String
syntaxError = "Usage: ttrack command\n\
              \\n\
              \commands:\n\
              \\t create [task]\t\t\t creates a new task\n\
              \\t start [task]\t\t\t starts tracking task\n\
              \\t end \t\t\t\t stops tracking task\n\
              \\t current \t\t\t get current task in progress\n\
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
            end
            return ()
        ["list"] -> do
            list
            return ()
        ["remove", n] -> do
            remove n
            return ()
        ["time", n] -> do
            t <- time n
            tell $ ["Time spent on task " ++ n ++ " is " ++ (renderSecs $ round t)]
            return ()
        _ -> do
            tell [syntaxError]
            return ()

--main :: IO ()
--main = withSocketsDo $ handleSqlError $
--    do  args <- getArgs
--        dbh <- connect dbname
--        (r,msg) <- runTrackerMonad (handleInput args) dbh
--        commit dbh
--        mapM_ putStrLn msg
--        disconnect dbh

 --with bracket. Maybe better for disconnecting?
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
                            putStrLn err
                            rollback dbh
                        Right (v,msg) -> mapM_ putStrLn msg

        finalize dbh = do disconnect dbh
        errorHandler :: SomeException -> IO ()
        errorHandler e = putStrLn $ "An error occured: " ++ show e



create :: String -> TrackerMonad Task
create name = do
    task <- createTask name
    tell $ ["Created task: " ++ name]
    return task

start :: String -> TrackerMonad Session
start name = do
    dbh <- ask
    t <- getTaskByName name
    s <- startSession t
    tell ["Started tracking " ++ name]
    return s
    `catchError` \e -> do
        liftIO $ putStrLn $ "No task with name " ++ name ++ " was found. Create new task? (y/n)"
        resp <- liftIO $ getLine
        if (map toLower resp == "y")
            then do create name
                    start name
            else do throwError "No action taken"


current :: TrackerMonad Task
current = do
    last <- getLastSession
    if isEnded last
        then throwError "No current task could be found"
        else do
            let task = sessTask last
            return $ task



end :: TrackerMonad Session
end = do
    lastSess <- getLastSession
    endSess <- endSession lastSess
    tell ["Ended task " ++ (show . taskName . sessTask) lastSess]
    return endSess


list :: TrackerMonad [Task]
list = do
    dbh <- ask
    tasks <- getTasks
    tell ["Listing tasks..."]
    mapM_ (\t -> tell ['\t':taskName t]) tasks
    return tasks


remove :: String -> TrackerMonad (Task, [Session])
remove name = do
    dbh <- ask
    task <- removeTask name
    tell ["Removing task " ++ name]
    sessions <- removeTaskSessions task
    tell ["Removing " ++ show (length sessions) ++ " session(s)"]
    return (task, sessions)


time :: String -> TrackerMonad NominalDiffTime
time n = do
    dbh <- ask
    task <- getTaskByName n
    sessions <- getTaskSessions task
    case sessions of
        [] -> return (0 :: NominalDiffTime)
        s -> do
            -- TODO. This will fail when a session is not ended. Fix by either refusing
            -- or calculating un-ended sessions with end = now. But notify the user
            durs <- liftIO $ mapM (sessDurationMonad) s
            let time = foldr (\x acc -> x+acc) 0 durs
            return time


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