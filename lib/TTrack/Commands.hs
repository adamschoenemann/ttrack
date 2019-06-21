module TTrack.Commands where

import           TTrack.DB
import           TTrack.Types
import           TTrack.Utils
import           TTrack.TimeUtils
import           TTrack.Config
import           System.Directory (doesFileExist, removeFile)
import           Control.Monad.Except
import           Data.Time
import           Data.Maybe.Extras (fromJustMsg)
import           System.Locale
import           Data.Char (toUpper, toLower)

create :: String -> TrackerMonad Task
create name = do
  last <- getLastSession
  if isEnded last
    then createNew
    else
      throwError
        $ OtherSessionStarted
        $ "A session is already in progress. Please close it \
                \before creating a new task."
  `catchError` errorHandler
  where
    errorHandler (NoLastSession msg) = createNew

    createNew = do
      task <- createTask name
      tell $ ["Created task: " ++ name]
      return task

start :: String -> Maybe String -> TrackerMonad Session
start name mbegin = do
  t <- getTaskByName name
  last <- getLastSession
  if not (isEnded last)
    then throwError
      $ OtherSessionStarted
      $ "Can't start a new session when session "
      ++ (taskName . sessTask $ last)
      ++ " is already open. Please close it first"
    else startSession' t mbegin
  `catchError` errorHandler
  where
    errorHandler (NoTaskFound msg) = createNew
    errorHandler (NoLastSession msg) = do
      t <- getTaskByName name
      startSession' t mbegin
    errorHandler e = throwError e

    createNew = do
      liftIO
        $ putStrLn
        $ "No task with name " ++ name ++ " was found. Create new task? (y/n)"
      resp <- liftIO $ getLine
      if map toLower resp == "y"
        then do
          create name
          start name mbegin
        else throwError $ OtherError $ "No action taken"

    startSession' :: Task -> Maybe String -> TrackerMonad Session
    startSession' t mbegin = do
      mbeginUtc <- maybe (pure Nothing) ((Just <$>) . parseTimeInput) mbegin
      s <- startSession t mbeginUtc
      let from = maybe "" (\b -> " from " ++ show b) mbeginUtc
      tell ["Started tracking " ++ name ++ from]
      return s

current :: TrackerMonad Task
current = do
  last <- getLastSession
  if isEnded last
    then throwError $ NoTaskFound $ "No current task could be found"
    else return $ sessTask last

stop :: TrackerMonad Session
stop = do
  lastSess <- getLastSession
  endSess <- endSession lastSess
  let (Just dur) = sessDuration endSess
  tellUsr
    $ "Session duration was "
    ++ renderDuration dur
    ++ ".\nIs this correct? (y/n)"
  resp <- liftIO getLine
  if isCorrect resp
    then do
      tell ["Ended task " ++ (show . taskName . sessTask) lastSess]
      return endSess
    else inputDuration endSess
  where
    isCorrect r
      | map toUpper r == "Y" = True
      | otherwise = False

    inputDuration sess = do
      tellUsr "Please input duration in format [hm]s e.g. 1h30m10s"
      durString <- liftIO getLine
      let durDiffTime = parseDurationToDiffTime durString
      case durDiffTime of
        Just d  ->
          setSessDuration sess d
        Nothing -> do
          liftIO
            $ putStrLn
              "Incorrect duration format. Please use hh:mm (e.g. 02:33)"
          inputDuration sess

    tellUsr = liftIO . putStrLn

report :: String -> TrackerMonad ()
report n = do
  task <- getTaskByName n
  sessions <- getTaskSessions task
  tz <- liftIO getCurrentTimeZone
  mapM_ (\x -> tell [showSess x tz]) sessions

list :: TrackerMonad [Task]
list = do
  tasks <- getTasks
  tell ["Listing tasks..."]
  mapM_ (\t -> tell ['\t':taskName t]) tasks
  return tasks

remove :: String -> TrackerMonad (Task, [Session])
remove name = do
  task <- removeTask name
  tell ["Removing task " ++ name]
  sessions <- removeTaskSessions task
  tell ["Removing " ++ show (length sessions) ++ " session(s)"]
  return (task, sessions)

timeInInterval :: String -> String -> String -> TrackerMonad NominalDiffTime
timeInInterval name from to = do
  froms <- parseTimeInput from
  tos <- parseTimeInput to
  t <- getTaskByName name
  ss <- getTaskSessionsInInterval t froms tos
  let time = sum $ map (toInteger . round . fromJustMsg "timeInInterval" . sessDuration) ss
  return (fromInteger time :: NominalDiffTime)

time :: String -> TrackerMonad NominalDiffTime
time n = do
  task <- getTaskByName n
  sessions <- getTaskSessions task
  -- TODO. This will fail when there is more than one un-ended session or
  -- if the un-ended session is not the last
  durs <- liftIO $ mapM sessDurationIO sessions
  --let time = sum $ map (toInteger . round . fromJust . sessDuration) ss
  return $ sum durs

duration :: TrackerMonad NominalDiffTime
duration = do
  sess <- getLastSession
  if isEnded sess
    then throwError $ NoCurrentSession "No session is currently in progress"
    else liftIO $ sessDurationIO sess

reset = do
  f <- doesFileExist dbname
  when f $ removeFile dbname
