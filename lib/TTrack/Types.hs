module TTrack.Types (module TTrack.Types, tell) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Writer

import Data.Maybe (fromJust)
import Data.Monoid
import Data.Time

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Locale hiding (defaultTimeLocale)
import System.Time.Utils (renderSecs)

import TTrack.TimeUtils (readSeconds)

data Task =
  Task
    { taskId :: Integer
    , taskName :: String
    }
  deriving (Show)

data Session =
  Session
    { sessId :: Integer
    , sessTask :: Task
    , sessStart :: UTCTime
    , sessEnd :: Maybe UTCTime
    }
  deriving (Show)

data TTError
  = NoTaskFound String
  | NoSessionFound String Task
  | UnexpectedSqlResult String
  | TaskAlreadyExists String
  | OtherSessionStarted String
  | NoCurrentSession String
  | NoLastSession String
  | OtherError String
  deriving (Show)

unwrapTTError :: TTError -> String
unwrapTTError err @ (NoTaskFound s) = s
unwrapTTError err @ (NoSessionFound s _) = s
unwrapTTError err @ (UnexpectedSqlResult s) = show err
unwrapTTError err @ (TaskAlreadyExists s) = s
unwrapTTError err @ (OtherSessionStarted s) = s
unwrapTTError err @ (NoCurrentSession s) = s
unwrapTTError err @ (OtherError s) = show err

isEnded :: Session -> Bool
isEnded s = not $ (sessEnd s) == Nothing

sessDuration :: Session -> Maybe NominalDiffTime
sessDuration sess =
  case end of
    Nothing -> Nothing
    Just endTime -> Just $ diffUTCTime endTime startTime
  where
    end = sessEnd sess

    startTime = sessStart sess

showSess :: Session -> TimeZone -> String
showSess s tz =
  (showStart s)
  ++ " | "
  ++ (showEnd s)
  ++ " | "
  ++ (show $ readSeconds $ round $ fromJust $ sessDuration s)
  where
    format = "%F %T %z"

    dtl = defaultTimeLocale

    showEnd s =
      case (sessEndZoned s tz) of
        Nothing -> "Unended"
        Just end -> formatTime dtl format $ end

    showStart s = formatTime dtl format $ sessStartZoned s tz

sessStartZoned :: Session -> TimeZone -> ZonedTime
sessStartZoned s tz = utcToZonedTime tz $ sessStart s

sessEndZoned :: Session -> TimeZone -> Maybe ZonedTime
sessEndZoned s tz = utcToZonedTime tz <$> sessEnd s

sessDurationIO :: Session -> IO NominalDiffTime
sessDurationIO sess = do
  let dur = sessDuration sess
  case dur of
    Just x  -> return x
    Nothing -> do
      cur <- getCurrentTime
      return $ diffUTCTime cur (sessStart sess)

taskFromSql :: [SqlValue] -> Task
taskFromSql [id, name] = Task (fromSql id) (fromSql name)

sessFromSql :: [SqlValue] -> Task -> Session
sessFromSql [id, _, start, end] task =
  Session (fromSql id) task (fromSql start) (fromSql end)

sessToSql :: Session -> [SqlValue]
sessToSql sess = [ toSql $ taskId $ sessTask sess
                 , toSql $ sessStart sess
                 , toSql $ sessEnd sess]

type TrackerMonad a =
  WriterT [String] (ReaderT Connection (ExceptT TTError IO)) a

runTrackerMonad :: TrackerMonad a
                -> Connection
                -> IO (Either TTError (a, [String]))
runTrackerMonad m conn = runExceptT (runReaderT (runWriterT m) conn)
