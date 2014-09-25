module TimeTrackerDB where
import TimeTrackerTypes
import TimeTrackerUtils
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
                y -> throwError $ UnexpectedSqlResult $ "addTask: unexpected result: " ++ show y
        _ -> throwError $ TaskAlreadyExists $ "A task with name " ++ name ++ " already exists."


startSession :: Task -> TrackerMonad Session
startSession task = do
    dbh <- ask
    start <- liftIO getCurrentTime
    liftIO $ run dbh "INSERT INTO sessions (taskId, start, end) VALUES (?, ?, ?)"
            [toSql $ taskId task, toSql start, toSql (Nothing :: Maybe UTCTime)]
    r <- liftIO $ quickQuery' dbh "SELECT * FROM sessions WHERE start=?" [toSql start]
    case r of
        [x] -> return $ sessFromSql x task
        y -> throwError $ UnexpectedSqlResult $ "startSession: unexpected result: " ++ show y


getTaskSessionsInInterval :: Task -> UTCTime -> UTCTime -> TrackerMonad [Session]
getTaskSessionsInInterval task from to = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh
                "SELECT * FROM sessions WHERE taskId=? AND start > datetime(?) AND end < datetime(?)"
                [toSql $ taskId task, toSql $ utcToISO from, toSql $ utcToISO to]
    case r of
        [] -> throwError $ NoSessionFound
                ("No sessions found between " ++ (utcToISO from) ++ " and " ++ (utcToISO to)) task
        rows -> return $ map (`sessFromSql` task) rows



addSession :: Session -> TrackerMonad Session
addSession sess = do
    dbh <- ask
    liftIO $ run dbh "INSERT INTO sessions (taskId, start, end) VALUES (?, ?, ?)" $
                     sessToSql sess
    r <- liftIO $ quickQuery' dbh "SELECT id FROM sessions WHERE taskId=? AND start=? AND end=?"
                $ sessToSql sess
    case r of
        [[id]] -> return $ sess {sessId = fromSql id}
        y -> throwError $ UnexpectedSqlResult $ "addSession: unexpected result: " ++ show y


endSession :: Session -> TrackerMonad Session
endSession sess = do
    dbh <- ask
    r <- liftIO $ doSql dbh
    case r of
        [] -> throwError $ NoSessionFound "endSession: unexpected empty result" (sessTask sess)
        [[end]] -> return $ sess {sessEnd = fromSql end}
        y -> throwError $ UnexpectedSqlResult $ "endSession: unexpected result: " ++ show y
    where doSql dbh = do
            time <- getCurrentTime
            run dbh "UPDATE sessions SET end=? WHERE id=?" [toSql time, toSql . sessId $ sess]
            r <- quickQuery' dbh "SELECT end FROM sessions WHERE end=?" [toSql time]
            return r

setSessDuration :: Session -> NominalDiffTime -> TrackerMonad Session
setSessDuration sess dur = do
    dbh <- ask
    liftIO $ run dbh "UPDATE sessions SET end=? WHERE id=?" [toSql end, toSql $ sessId sess]
    --r <- liftIO $ quickQuery' dbh "SELECT * FROM sessions WHERE id=?" [toSql $ sessId sess]
    return $ sess {sessEnd = Just end}
    where
        end = addUTCTime dur (sessStart sess)

getTaskById :: Integer -> TrackerMonad Task
getTaskById id = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh "SELECT * FROM tasks WHERE id=?" [toSql id]
    case r of
        [] -> throwError $ NoTaskFound $ "No task with id " ++ show id ++ " was found"
        [row@[id, name]] -> return $ taskFromSql row
        x -> throwError $ UnexpectedSqlResult $ "unexpected result in getTaskById: " ++ show x


getTaskByName :: String -> TrackerMonad Task
getTaskByName name = do
        dbh <- ask
        r <- liftIO $ quickQuery' dbh "SELECT * FROM tasks WHERE name=?" [toSql name]
        case r of
            [row] -> return $ taskFromSql row
            [] -> throwError $ NoTaskFound $ "No task with name " ++ name ++ " was found"
            y -> throwError $ UnexpectedSqlResult $ "Unexpected result in getTaskByName: " ++ show y

getLastSession :: TrackerMonad Session
getLastSession = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh "SELECT * FROM sessions ORDER BY datetime(start) DESC LIMIT 1" []
    case r of
        [] -> throwError $ NoLastSession $ "Error in getLastSession: No sessions found"
        [row@[id, tid, start, end]] -> do
            task <- getTaskById (fromSql tid)
            return $ sessFromSql row task


getTasks :: TrackerMonad [Task]
getTasks = do
    dbh <- ask
    r <- liftIO $ quickQuery' dbh "SELECT * FROM tasks" []
    case r of
        [] -> throwError $ NoTaskFound $ "No tasks found."
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
        --[] -> throwError $ NoSessionFound $ "No sessions found for task " ++ taskName task
        [] -> return []
        rows -> return $ map (`sessFromSql` task) rows

removeTaskSessions :: Task -> TrackerMonad [Session]
removeTaskSessions task = do
    sessions <- getTaskSessions task
    case sessions of
        [] -> return []
        _ -> do
                dbh <- ask
                liftIO $ run dbh "DELETE FROM sessions WHERE taskId=?" [toSql $ taskId task]
                return sessions



