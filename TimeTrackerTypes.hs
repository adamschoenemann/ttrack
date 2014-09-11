module TimeTrackerTypes where
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Time
import System.Locale

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


taskFromSql :: [SqlValue] -> Task
taskFromSql [id, name] = Task (fromSql id) (fromSql name)

sessionFromSql :: [SqlValue] -> Task -> Session
sessionFromSql [id, _, start, end] task = Session (fromSql id) task (fromSql start) (fromSql end)

type TrackerMonad a = ReaderT Connection (WriterT [String] IO) a

runTrackerMonad :: TrackerMonad a -> Connection -> IO (a, [String])
runTrackerMonad m conn = runWriterT $ runReaderT m conn