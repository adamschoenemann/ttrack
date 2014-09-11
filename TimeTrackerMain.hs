{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TimeTrackerMain where

import TimeTrackerDB
import TimeTrackerTypes
import System.Directory (removeFile, doesFileExist)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Time
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Char (toLower)
import System.Environment
import Network.Socket (withSocketsDo)





dbname :: String
dbname = "hstt.db"

{-  Usage: ttrack command
    commands:
        create [task]           Creates a new task
        start [task]            starts tracking task
        stop [task]             stops tracking task
        list                    Lists all tasks
        remove [task]           Removes task and all sessions for that task
-}

syntaxError :: String
syntaxError = "Usage: ttrack command\n\
              \\n\
              \commands:\n\
              \\t create [task]\t\t\t creates a new task\n\
              \\t start [task]\t\t\t starts tracking task\n\
              \\t stop [task]\t\t\t stops tracking task\n\
              \\t list \t\t\t\t lists all tasks\n\
              \\t remove [task] \t\t\t removes task and all sessions for that task"

handleInput :: [String] -> TrackerMonad (Maybe Task)
handleInput args = do
    case args of
        ["create", n] -> do t <- create n
                            return $ Just t
        _ -> do tell [syntaxError]
                return $ Nothing

main :: IO ()
main = withSocketsDo $ handleSqlError $
    do  args <- getArgs
        dbh <- connect dbname
        (r,msg) <- runTrackerMonad (handleInput args) dbh
        commit dbh
        mapM_ putStrLn msg
        disconnect dbh

create :: String -> TrackerMonad Task
create name = do
    dbh <- ask
    t <- liftIO . addTask dbh $ Task 0 name
    tell ["Created task: " ++ name]
    return t

--start :: IConnection conn => conn -> String -> IO ()
--start dbh tName = do
--    r <- quickQuery' dbh "SELECT * FROM tasks WHERE name=?" [toSql tName]
--    case r of
--        [] -> putStrLn $ "No task with name " ++ tName ++ " was found. Create new task? (y/n)"
--        resp <- getStrLn
--        handleResponse resp

--    where handleResponse r
--        | map toLower r == "y" = create



test1 = do
    dbh <- connect dbname
    t <- addTask dbh $ Task {taskId = 0, taskName = "myTask"}
    now <- getCurrentTime
    let s = newSess t now
    addSession dbh s
    commit dbh
    disconnect dbh
    return ()
    where newSess task start = Session {sessId = 0
                                       ,sessTask = task
                                       ,sessStart = start
                                       ,sessEnd = Nothing
                                       }

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