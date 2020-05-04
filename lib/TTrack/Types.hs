{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TTrack.Types
  ( module TTrack.Types
  , tell) where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Writer

import           Data.Maybe (fromMaybe)
import           Data.Maybe.Extras (fromJustMsg)
import           Data.Monoid
import           Data.Time

import           Database.HDBC
import           Database.HDBC.Sqlite3

import           System.Time.Utils (renderSecs)

import           TTrack.TimeUtils (readHoursRoundQuarters, readSeconds)

data Task = Task { taskId :: Integer, taskName :: String }
  deriving (Show, Eq)

data Session =
  Session { sessId :: Integer
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
  deriving (Show, Eq)

data GroupBy
  = DayGroup
  | NoGroup
  deriving (Eq)

instance Show GroupBy where
  show DayGroup = "day"
  show NoGroup = "none"

instance Read GroupBy where
  readsPrec _ "day" = [(DayGroup, "")]
  readsPrec _ "none" = [(NoGroup, "")]

   -- Make Either ErrorT a an instance of monoid for concatenation.
   -- WITH short-circuiting
instance (Monoid a) => Monoid (Either e a) where
  mempty = (Right mempty)

  mappend (Left x) _ = Left x
  mappend _ (Left x) = Left x
  mappend (Right a) (Right b) = Right
    (a `mappend` b)

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

sessDurationWithNow :: UTCTime -> Session -> NominalDiffTime
sessDurationWithNow now sess = diffUTCTime endTime startTime
  where
    endTime = fromMaybe now $ sessEnd sess

    startTime = sessStart sess

sessDuration :: Session -> Maybe NominalDiffTime
sessDuration sess = case end of
  Nothing -> Nothing
  Just endTime -> Just $ diffUTCTime endTime startTime
  where
    end = sessEnd sess

    startTime = sessStart sess

type SessTime = (UTCTime, Maybe UTCTime, Maybe NominalDiffTime)

sessToSessTime :: Session -> SessTime
sessToSessTime s = (sessStart s, sessEnd s, sessDuration s)

sessStartZoned :: Session -> TimeZone -> ZonedTime
sessStartZoned s tz = utcToZonedTime tz $ sessStart s

sessEndZoned :: Session -> TimeZone -> Maybe ZonedTime
sessEndZoned s tz = utcToZonedTime tz <$> sessEnd s

sessDurationIO :: Session -> IO NominalDiffTime
sessDurationIO sess = do
  let dur = sessDuration sess
  case dur of
    Just x -> return x
    Nothing -> do
      cur <- getCurrentTime
      return $ diffUTCTime cur (sessStart sess)

taskFromSql :: [SqlValue] -> Task
taskFromSql [id, name] =
  Task (fromSql id) (fromSql name)

sessFromSql :: [SqlValue] -> Task -> Session
sessFromSql [id, _, start, end] task =
  Session (fromSql id) task (fromSql start) (fromSql end)

sessToSql :: Session -> [SqlValue]
sessToSql sess =
  [ toSql $ taskId $ sessTask sess
  , toSql $ sessStart sess
  , toSql $ sessEnd sess
  ]

type TrackerConfig = Session

-- data TrackerConfig
--   = TrackerConfig
--     { _connection :: Connection
--     , _locale :: TimeLocale
--     }
newtype TrackerMonad a =
  TrackerMonad { unTrackerMonad
                   :: WriterT [String] (ReaderT Connection (ExceptT TTError IO)) a
               }
  deriving (Functor, Applicative, Monad, MonadWriter [String]
          , MonadReader Connection, MonadError TTError, MonadIO)

getTTTimeZone :: TrackerMonad TimeZone
getTTTimeZone = liftIO getCurrentTimeZone

getTTCurrentTime :: TrackerMonad UTCTime
getTTCurrentTime = liftIO getCurrentTime

runTrackerMonad
  :: TrackerMonad a -> Connection -> IO (Either TTError (a, [String]))
runTrackerMonad m conn = runExceptT
  (runReaderT (runWriterT $ unTrackerMonad m) conn)
