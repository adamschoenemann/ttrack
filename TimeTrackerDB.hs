module TimeTrackerDB where
import TimeTrackerTypes
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when)



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
                    \end TEXT\
                    \)" []
                return ()
        commit dbh

addTask :: IConnection conn => conn -> Task -> IO Task
addTask dbh task = handleSql errorHandler $
    do
        run dbh "INSERT INTO tasks (name) VALUES (?)" [toSql $ taskName task]
        id <- quickQuery' dbh "SELECT id FROM tasks WHERE name=?" [toSql $ taskName task]
        case id of
            [[x]] -> return $ task {taskId = fromSql x}
            y -> fail $ "addTask: unexpected result: " ++ show y
        where errorHandler e =
                do fail $ "Error adding task. Does a task with the same name already exist?\n" ++ show e

addSession :: IConnection conn => conn -> Session -> IO Session
addSession dbh sess = handleSql errorHandler $
    do
        run dbh "INSERT INTO sessions (taskId, start, end) VALUES (?, ?, ?)"
                [toSql $ taskId $ sessTask sess, toSql $ sessStart sess, toSql $ sessEnd sess]
        id <- quickQuery' dbh "SELECT id FROM sessions WHERE start=?" [toSql $ sessStart sess]
        case id of
            [[x]] -> return $ sess {sessId = fromSql x}
            y -> fail $ "addSession: unexpected result: " ++ show y
    where errorHandler e =
            do   fail $
                    "Error adding session; A session with same start or end time already exists\n"
                    ++ show e


getTaskByName :: IConnection conn => conn -> String -> IO Task
getTaskByName dbh name = handleSql errorHandler $
    do
        r <- quickQuery' dbh "SELECT * FROM tasks WHERE name=?" [toSql name]
        case r of
            [[id, name]] -> return $ Task (fromSql id) (fromSql name)
            y -> fail $ "getTaskByName: No task with that name found:" ++ show y
    where errorHandler e = do fail $ "SQL error in getTaskByName: " ++ show e

--getSessionsByTask :: IConnection conn => conn -> Task -> IO [Session]
--getSessionsByTask dbh task = handleSql errorHandler $
--    do
--        r <- quickQuery' dbh "SELECT * FROM sessions WHERE taskId=?" [toSql $ taskId task]
--        case r of
--            [row] -> map


