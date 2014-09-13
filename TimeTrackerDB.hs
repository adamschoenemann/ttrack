module TimeTrackerDB where
import TimeTrackerTypes
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when)
import Control.Exception
import Prelude hiding (handle)
import Data.Time
import System.Locale



connect :: FilePath -> IO Connection
connect fp = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    return dbh


prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
    do  tables <- getTables dbh
        when (not ("tasks" `elem` tables)) $
            do  run dbh
                    "CREATE TABLE tasks (\
                    \id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \name TEXT NOT NULL UNIQUE\
                    \)" []
                return ()
        when (not $ "tasks" `elem` tables) $
            do  run dbh
                    "CREATE TABLE sessions (\
                    \id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \taskId INTEGER NOT NULL,\
                    \start TEXT NOT NULL UNIQUE,\
                    \end TEXT UNIQUE\
                    \)" []
                return ()
        commit dbh

addTask :: IConnection conn => conn -> Task -> IO (Either String Task)
addTask dbh task =
    do
        r <- quickQuery' dbh "SELECT name FROM tasks WHERE name=?" [toSql $ taskName task]
        case r of
            [] -> do
                run dbh "INSERT INTO tasks (name) VALUES (?)" [toSql $ taskName task]
                id <- quickQuery' dbh
                    "SELECT id FROM tasks WHERE name=?" [toSql $ taskName task]
                case id of
                    [[x]] -> return $ Right $ task {taskId = fromSql x}
                    y -> return $ Left $ "addTask: unexpected result: " ++ show y
            _ -> return $ Left $ "A task with name " ++ taskName task ++ " already exists."

startSession :: IConnection conn => conn -> Task -> IO Session
startSession dbh task = do
    start <- getCurrentTime
    run dbh "INSERT INTO sessions (taskId, start, end) VALUES (?, ?, ?)"
            [toSql $ taskId task, toSql start, toSql (Nothing :: Maybe UTCTime)]
    r <- quickQuery' dbh "SELECT * FROM sessions WHERE start=?" [toSql start]
    case r of
        [x] -> return $ sessionFromSql x task
        y -> fail $ "startSession: unexpected result: " ++ show y


endSession :: IConnection conn => conn -> Session -> IO Session
endSession dbh sess = do
    time <- getCurrentTime
    putStrLn $ show time
    run dbh "UPDATE sessions SET end=? WHERE id=?" [toSql time, toSql . sessId $ sess]
    r <- quickQuery' dbh "SELECT end FROM sessions WHERE end=?" [toSql time]
    case r of
        [] -> fail $ "endSession: unexpected empty result"
        [[end]] -> return $ sess {sessEnd = fromSql end}
        y -> fail $ "endSession: unexpected result: " ++ show y


--addSession :: IConnection conn => conn -> Session -> IO Session
--addSession dbh sess = handleSql errorHandler $
--    do
--        run dbh "INSERT INTO sessions (taskId, start, end) VALUES (?, ?, ?)"
--                [toSql $ taskId $ sessTask sess, toSql $ sessStart sess, toSql $ sessEnd sess]
--        id <- quickQuery' dbh "SELECT id FROM sessions WHERE start=?" [toSql $ sessStart sess]
--        case id of
--            [[x]] -> return $ sess {sessId = fromSql x}
--            y -> fail $ "addSession: unexpected result: " ++ show y
--    where errorHandler e =
--            do   fail $
--                    "Error adding session; A session with same start or end time already exists\n"
--                    ++ show e


getTaskById :: IConnection conn => conn -> Integer -> IO (Either String Task)
getTaskById dbh id = do
    r <- quickQuery' dbh "SELECT * FROM tasks WHERE id=?" [toSql id]
    case r of
        [] -> return $ Left $ "No task with id " ++ show id ++ " was found"
        [row@[id, name]] -> return $ Right $ taskFromSql row
        x -> fail $ "unexpected result in getTaskById"


getTaskByName :: IConnection conn => conn -> String -> IO (Maybe Task)
getTaskByName dbh name = handleSql errorHandler $
    do
        r <- quickQuery' dbh "SELECT * FROM tasks WHERE name=?" [toSql name]
        case r of
            [row] -> return $ Just $ taskFromSql row
            y -> return Nothing
    where errorHandler e = do fail $ "SQL error in getTaskByName: " ++ show e

getLastSession :: IConnection conn => conn -> IO (Maybe Session)
getLastSession dbh = do
    r <- quickQuery' dbh "SELECT * FROM sessions ORDER BY datetime(start) DESC LIMIT 1" []
    case r of
        [] -> return Nothing
        [row@[id, tid, start, end]] -> do
            task <- getTaskById dbh (fromSql tid)
            case task of
                Right t -> return $ Just $ sessionFromSql row t
                Left x -> return Nothing

getTasks :: IConnection conn => conn -> IO (Maybe [Task])
getTasks dbh = do
    r <- quickQuery' dbh "SELECT * FROM tasks" []
    case r of
        [] -> return Nothing
        x -> return $ Just $ map taskFromSql x

removeTask :: IConnection conn => conn -> String -> IO (Maybe Task)
removeTask dbh n = do
    task <- getTaskByName dbh n
    case task of
        Nothing -> return Nothing
        Just t -> do
            run dbh "DELETE FROM tasks WHERE id=?" [toSql $ taskId t]
            return task


getTaskSessions :: IConnection conn => conn -> Task -> IO (Maybe [Session])
getTaskSessions dbh task = do
    r <- quickQuery' dbh "SELECT * FROM sessions WHERE taskId=?" [toSql $ taskId task]
    case r of
        [] -> return Nothing
        rows -> do
            let sessions = map (`sessionFromSql` task) rows
            return $ Just sessions

removeTaskSessions :: IConnection conn => conn -> Task -> IO (Maybe [Session])
removeTaskSessions dbh task = do
    sessions <- getTaskSessions dbh task
    case sessions of
        Nothing -> return Nothing
        Just x -> do
            run dbh "DELETE FROM sessions WHERE taskId=?" [toSql $ taskId task]
            return $ Just x




--getSessionsByTask :: IConnection conn => conn -> Task -> IO [Session]
--getSessionsByTask dbh task = handleSql errorHandler $
--    do
--        r <- quickQuery' dbh "SELECT * FROM sessions WHERE taskId=?" [toSql $ taskId task]
--        case r of
--            [row] -> map


