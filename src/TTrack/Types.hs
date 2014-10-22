module TTrack.Types
    ( module TTrack.Types
    , tell
    ) where

import TTrack.TimeUtils (readSeconds)
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import Data.Time
import System.Locale
import System.Time.Utils (renderSecs)
import Data.Monoid
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))

data Task = Task {
     taskId :: Integer
    ,taskName :: String
} deriving (Show)

data Session = Session {
     sessId :: Integer
    ,sessTask :: Task
    ,sessStart :: UTCTime
    ,sessEnd :: Maybe UTCTime
} deriving (Show)

data TTError = NoTaskFound String
             | NoSessionFound String Task
             | UnexpectedSqlResult String
             | TaskAlreadyExists String
             | OtherSessionStarted String
             | NoCurrentSession String
             | NoLastSession String
             | OtherError String
             deriving (Show)

instance Error TTError where
    noMsg = OtherError "An error occured"
    strMsg s = OtherError s

unwrapTTError :: TTError -> String
unwrapTTError err@(NoTaskFound s) = s
unwrapTTError err@(NoSessionFound s _) = s
unwrapTTError err@(UnexpectedSqlResult s) = show err
unwrapTTError err@(TaskAlreadyExists s) = s
unwrapTTError err@(OtherSessionStarted s) = s
unwrapTTError err@(NoCurrentSession s) = s
unwrapTTError err@(OtherError s) = show err


isEnded :: Session -> Bool
isEnded s = not $ (sessEnd s) == Nothing


sessDuration :: Session -> Maybe NominalDiffTime
sessDuration sess = case end of
    Nothing -> Nothing
    Just endTime -> Just $ diffUTCTime endTime startTime
    where
        end = sessEnd sess
        startTime = sessStart sess

showSess :: Session -> TimeZone -> String
showSess s tz = (showStart s) ++ " | " ++ (showEnd s) ++ " | " ++
                (show $ readSeconds $ round $ fromJust $ sessDuration s)
                    where
                        format = "%F %T %z"
                        dtl = defaultTimeLocale
                        showEnd s = case (sessEndZoned s tz) of
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
        Just x -> return x
        Nothing -> do
            cur <- getCurrentTime
            return $ diffUTCTime cur (sessStart sess)


taskFromSql :: [SqlValue] -> Task
taskFromSql [id, name] = Task (fromSql id) (fromSql name)

sessFromSql :: [SqlValue] -> Task -> Session
sessFromSql [id, _, start, end] task = Session (fromSql id) task (fromSql start) (fromSql end)

sessToSql :: Session -> [SqlValue]
sessToSql sess = [toSql $ taskId $ sessTask sess
                    ,toSql $ sessStart sess
                    ,toSql $ sessEnd sess]

type TrackerMonad a = WriterT [String] (ReaderT Connection (ErrorT TTError IO)) a

runTrackerMonad :: TrackerMonad a -> Connection -> IO (Either TTError (a, [String]))
runTrackerMonad m conn = runErrorT (runReaderT (runWriterT m) conn)