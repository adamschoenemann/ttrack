{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import TTrack.CLI
import TTrack.Types
import TTrack.Config
import TTrack.DB
import System.Directory
import System.FilePath
import System.Environment
import Network.Socket (withSocketsDo)
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Exception
import Control.Monad
import Prelude hiding (handle, catch)



main :: IO ()
main = withSocketsDo $ handle errorHandler $ bracket acquire finalize runCommand

runCommand :: Connection -> IO ()
runCommand dbh = do withTransaction dbh doSql
    where doSql dbh = do
            args <- getArgs
            r <- runTrackerMonad (handleInput args) dbh
            case r of
                Left err -> do
                    putStrLn $ unwrapTTError err
                    rollback dbh
                Right (v,msg) -> mapM_ putStrLn msg

acquire :: IO (Connection)
acquire = do
    dir <- getDir
    dirExists <- doesDirectoryExist dir
    when (not $ dirExists)
        (createDirectory dir)
    dbh <- connect $ dir </> dbname
    return dbh

finalize :: Connection -> IO ()
finalize dbh = disconnect dbh

errorHandler :: SomeException -> IO ()
errorHandler e = putStrLn $ "An error occured: " ++ show e

getDir :: IO (String)
getDir = do
    ttrackDir <- lookupEnv dirEnvVar
    case ttrackDir of
        Nothing -> do
            defaultDir <- getAppUserDataDirectory appname
            putStrLn $ "Environment variable " ++ dirEnvVar ++
                " not set. Defaulting to " ++ defaultDir
            return defaultDir
        Just dir -> return dir



--showDB = do
--    dbh <- connect dbname
--    tasks <- quickQuery' dbh "SELECT * FROM tasks" []
--    sessions <- quickQuery' dbh "SELECT * FROM sessions" []

--    putStrLn $ show tasks
--    putStrLn $ show sessions