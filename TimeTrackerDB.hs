module TimeTrackerDB where
import TimeTrackerTypes
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Error
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

createTask :: String -> TrackerMonad Task
createTask name = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh "SELECT name FROM tasks WHERE name=?" [toSql $ name]
    case r of
        [] -> do
            liftIO $ run dbh "INSERT INTO tasks (name) VALUES (?)" [toSql $ name]
            id <- liftIO $ quickQuery' dbh
                "SELECT id FROM tasks WHERE name=?" [toSql $ name]
            case id of
                [[x]] -> return $ Task {taskId = fromSql x, taskName = name}
                y -> throwError $ "addTask: unexpected result: " ++ show y
        _ -> throwError $ "A task with name " ++ name ++ " already exists."

--addTask :: IConnection conn => conn -> Task -> IO (Either String Task)
--addTask dbh task =
--    do
--        r <- quickQuery' dbh "SELECT name FROM tasks WHERE name=?" [toSql $ taskName task]
--        case r of
--            [] -> do
--                run dbh "INSERT INTO tasks (name) VALUES (?)" [toSql $ taskName task]
--                id <- quickQuery' dbh
--                    "SELECT id FROM tasks WHERE name=?" [toSql $ taskName task]
--                case id of
--                    [[x]] -> return $ Right $ task {taskId = fromSql x}
--                    y -> return $ Left $ "addTask: unexpected result: " ++ show y
--            _ -> return $ Left $ "A task with name " ++ taskName task ++ " already exists."

startSession :: Task -> TrackerMonad Session
startSession task = do
    dbh <- ask
    start <- liftIO getCurrentTime
    liftIO $ run dbh "INSERT INTO sessions (taskId, start, end) VALUES (?, ?, ?)"
            [toSql $ taskId task, toSql start, toSql (Nothing :: Maybe UTCTime)]
    r <- liftIO $ quickQuery' dbh "SELECT * FROM sessions WHERE start=?" [toSql start]
    case r of
        [x] -> return $ sessionFromSql x task
        y -> throwError $ "startSession: unexpected result: " ++ show y


endSession :: Session -> TrackerMonad Session
endSession sess = do
    dbh <- ask
    r <- liftIO $ doSql dbh
    case r of
        [] -> throwError $ "endSession: unexpected empty result"
        [[end]] -> return $ sess {sessEnd = fromSql end}
        y -> throwError $ "endSession: unexpected result: " ++ show y
    where doSql dbh = do
            time <- getCurrentTime
            run dbh "UPDATE sessions SET end=? WHERE id=?" [toSql time, toSql . sessId $ sess]
            r <- quickQuery' dbh "SELECT end FROM sessions WHERE end=?" [toSql time]
            return r


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


getTaskById :: Integer -> TrackerMonad Task
getTaskById id = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh "SELECT * FROM tasks WHERE id=?" [toSql id]
    case r of
        [] -> throwError $ "No task with id " ++ show id ++ " was found"
        [row@[id, name]] -> return $ taskFromSql row
        x -> throwError $ "unexpected result in getTaskById: " ++ show x


getTaskByName :: String -> TrackerMonad Task
getTaskByName name = do
        dbh <- ask
        r <- liftIO $ quickQuery' dbh "SELECT * FROM tasks WHERE name=?" [toSql name]
        case r of
            [row] -> return $ taskFromSql row
            y -> throwError $ "Unexpected result in getTaskByName: " ++ show y

getLastSession :: TrackerMonad Session
getLastSession = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh "SELECT * FROM sessions ORDER BY datetime(start) DESC LIMIT 1" []
    case r of
        [] -> throwError "Error in getLastSession: No sessions found"
        [row@[id, tid, start, end]] -> do
            task <- getTaskById (fromSql tid)
            return $ sessionFromSql row task


getTasks :: TrackerMonad [Task]
getTasks = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh "SELECT * FROM tasks" []
    case r of
        [] -> throwError "No tasks found."
        x -> return $ map taskFromSql x

removeTask :: String -> TrackerMonad Task
removeTask n = do
    task <- getTaskByName n -- throws error here if task not found
    dbh <- ask
    liftIO $ run dbh "DELETE FROM tasks WHERE id=?" [toSql $ taskId task]
    return task


getTaskSessions :: Task -> TrackerMonad [Session]
getTaskSessions task = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh "SELECT * FROM sessions WHERE taskId=?" [toSql $ taskId task]
    case r of
        [] -> return []
        rows -> return $ map (`sessionFromSql` task) rows

removeTaskSessions :: Task -> TrackerMonad [Session]
removeTaskSessions task = do
    sessions <- getTaskSessions task
    case sessions of
        [] -> return []
        _ -> do
                dbh <- ask
                liftIO $ run dbh "DELETE FROM sessions WHERE taskId=?" [toSql $ taskId task]
                return sessions



