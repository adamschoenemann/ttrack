{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import           Control.Exception
import           Control.Monad

import           Database.HDBC
import           Database.HDBC.Sqlite3

import           Network.Socket (withSocketsDo)

import           Prelude hiding (catch, handle)

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath

import           TTrack.CLI
import           TTrack.Config
import           TTrack.DB
import           TTrack.Types

main :: IO ()
main =
  withSocketsDo $ handle errorHandler $ bracket acquire finalize runCommand

runCommand :: Connection -> IO ()
runCommand dbh = withTransaction dbh doSql
  where
    doSql dbh = do
      m <- parseCli
      r <- runTrackerMonad m dbh
      case r of
        Left err -> do
          putStrLn $ unwrapTTError err
          rollback dbh
        Right (v, msg) -> mapM_ putStrLn msg

acquire :: IO Connection
acquire = do
  dir <- getDir
  dirExists <- doesDirectoryExist dir
  unless dirExists (createDirectory dir)
  connect $ dir </> dbname

finalize :: Connection -> IO ()
finalize = disconnect

errorHandler :: SomeException -> IO ()
errorHandler e
  | Just e' <- fromException e :: Maybe ExitCode =
    case e' of
      ExitSuccess -> pure ()
      ExitFailure code -> exitFailure
  | otherwise = putStrLn $ "An error occurred: " ++ show e

getDir :: IO String
getDir = do
  ttrackDir <- lookupEnv dirEnvVar
  case ttrackDir of
    Nothing -> do
      defaultDir <- getAppUserDataDirectory appname
      putStrLn
        $ "Environment variable "
        ++ dirEnvVar
        ++ " not set. Defaulting to "
        ++ defaultDir
      return defaultDir
    Just dir -> return dir
