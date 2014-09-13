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
              \\t end \t\t\t stops tracking task\n\
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
            current
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
            time n
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
    where acquire =  do dbh <- connect dbname
                        return dbh

          proc dbh =
                do  withTransaction dbh doSql
                        where doSql dbh =
                                do  args <- getArgs
                                    (r,msg) <- runTrackerMonad (handleInput args) dbh
                                    commit dbh
                                    mapM_ putStrLn msg

          finalize dbh = do disconnect dbh
          errorHandler :: SomeException -> IO ()
          errorHandler e = putStrLn $ "An error occured: " ++ show e




--create :: String -> TrackerMonad Task
--create name = do
--    dbh <- ask
--    r <- liftIO . addTask dbh $ Task 0 name
--    tell $ ["Created task" ++ name]
--    return r

create :: String -> TrackerMonad (Maybe Task)
create name = do
    dbh <- ask
    r <- liftIO . addTask dbh $ Task 0 name
    case r of
        (Left x) -> do
            tell [x]
            return Nothing
        (Right t) -> do
            tell $ ["Created task: " ++ name]
            return $ Just t

start :: String -> TrackerMonad (Maybe Session)
start name = do
    dbh <- ask
    r <- liftIO $ getTaskByName dbh name
    case r of
        Nothing -> do
            liftIO $ putStrLn $ "No task with name " ++ name ++ " was found. Create new task? (y/n)"
            resp <- liftIO $ getLine
            if (map toLower resp == "y")
                then do create name
                        start name
                else do tell ["No action taken"]
                        return Nothing
        Just t -> do
            s <- liftIO $ startSession dbh $ t
            tell ["Started tracking " ++ name]
            return $ Just s

current :: TrackerMonad (Maybe Session)
current = do
    dbh <- ask
    last <- liftIO $ getLastSession dbh
    case last of
        Nothing -> do
            tell ["No sessions have been opened"]
            return Nothing
        Just sess -> do
            let end = sessEnd sess
            case end of
                Nothing -> do -- Session is not ended == its task is current
                    let task = sessTask sess
                    tell ["Current task is " ++ taskName task]
                    return $ Just $ sess
                Just e -> do -- The last session is ended == no task is in progress
                    tell ["No current task could be found"]
                    return Nothing

end :: TrackerMonad (Maybe Session)
end = do
    curSess <- current
    case curSess of
        Nothing -> do
            tell ["No current task to end"]
            return Nothing
        Just s -> do
            dbh <- ask
            endSess <- liftIO $ endSession dbh s
            tell ["Ended task " ++ (show . taskName . sessTask) endSess]
            return $ Just endSess


list :: TrackerMonad (Maybe [Task])
list = do
    dbh <- ask
    tasks <- liftIO $ getTasks dbh
    case tasks of
        Nothing -> do
            tell ["No tasks found"]
            return Nothing
        Just x -> do
            tell ["Listing tasks..."]
            mapM_ (\t -> tell ['\t':taskName t]) x
            return $ Just x

remove :: String -> TrackerMonad (Maybe Task, Maybe [Session])
remove name = do
    dbh <- ask
    task <- liftIO $ removeTask dbh name
    case task of
        Nothing -> do
            tell ["No task of that name found."]
            return (Nothing, Nothing)
        Just t -> do
            tell ["Removing task " ++ name]
            sessions <- liftIO $ removeTaskSessions dbh t
            case sessions of
                Nothing -> do
                    tell ["No sessions associated with task " ++ name]
                    return (Just t, Nothing)
                Just s -> do
                    tell ["Removing " ++ show (length s) ++ " session(s)"]
                    return (Just t, Just s)

time :: String -> TrackerMonad (Maybe NominalDiffTime)
time n = do
    dbh <- ask
    task <- liftIO $ getTaskByName dbh n
    case task of
        Nothing -> do
            tell ["No task with name " ++ n ++ " was found"]
            return Nothing
        Just t -> do
            sessions <- liftIO $ getTaskSessions dbh t
            case sessions of
                Nothing -> do
                    tell ["Task has no sessions, thus no time spent on it"]
                    return $ Just 0
                Just s -> do
                    -- TODO. This will fail when a session is not ended. Fix by either refusing
                    -- or calculating unded sessions with end = now. But notify the user
                    tell ["Calculating time"]
                    let durs = map (sessDuration) s
                    let time = foldr (\(Just x) acc -> x+acc) 0 durs
                    tell $ ["Time spent on task " ++ n ++ " is " ++ (renderSecs $ round time)]
                    return $ Just time



--test1 = do
--    dbh <- connect dbname
--    t <- addTask dbh $ Task {taskId = 0, taskName = "myTask"}
--    now <- getCurrentTime
--    let s = newSess t now
--    addSession dbh s
--    commit dbh
--    disconnect dbh
--    return ()
--    where newSess task start = Session {sessId = 0
--                                       ,sessTask = task
--                                       ,sessStart = start
--                                       ,sessEnd = Nothing
--                                       }

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