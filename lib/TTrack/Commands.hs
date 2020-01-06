module TTrack.Commands where

import           Control.Monad.Except

import           Data.Char (toLower, toUpper)
import           Data.Maybe.Extras (fromJustMsg)
import           Data.Time

import           System.Directory (doesFileExist, removeFile)

import           TTrack.Config
import           TTrack.DB
import           TTrack.TimeUtils
import           TTrack.Types
import           TTrack.Utils

create :: String -> TrackerMonad Task
create name = do
  last <- getLastSession
  if isEnded last
    then createNew
    else throwError
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
  tz <- getTTTimeZone
  t <- getTaskByName name
  last <- getLastSession
  if not (isEnded last)
    then throwError
      $ OtherSessionStarted
      $ "Can't start a new session when session "
      ++ (taskName . sessTask $ last)
      ++ " is already open. Please close it first"
    else startSession' tz t mbegin `catchError` errorHandler tz
  where
    errorHandler _ (NoTaskFound msg) = createNew
    errorHandler tz (NoLastSession msg) = do
      t <- getTaskByName name
      startSession' tz t mbegin
    errorHandler _ e = throwError e

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

    startSession' tz t mbegin = do
      mbeginUtc <- maybe (pure Nothing) ((Just <$>) . parseTimeInput tz) mbegin
      s <- startSession t mbeginUtc
      let from = maybe "" (\b -> " from " ++ show b) mbeginUtc
      tell ["Started tracking " ++ name ++ from]
      return s

current :: TrackerMonad Task
current = do
  last <- getLastUnendedSession
  if isEnded last
    then throwError $ NoTaskFound "No current task could be found"
    else do
      tell ["Current task is " ++ taskName (sessTask last)]
      pure $ sessTask last

cancel :: TrackerMonad ()
cancel = do
  last <- getLastUnendedSession
  if isEnded last
    then throwError $ NoTaskFound $ "No current task could be found"
    else removeSessionById (sessId last) >> pure ()

stop :: TrackerMonad Session
stop = do
  lastSess <- getLastSession
  tz <- getTTTimeZone
  let start = sessStartZoned lastSess tz
  endSess <- endSession lastSess
  let (Just dur) = sessDuration endSess
  tellUsr
    $ "Session started at "
    ++ show start
    ++ " and duration was "
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
        Just d -> setSessDuration sess d
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
  tz <- getTTTimeZone
  mapM_ (\x -> tell [showSess x tz]) sessions

purge :: String -> TrackerMonad ()
purge n = do
  task <- getTaskByName n
  purgeSessionsOfTaskId (taskId task)

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
  tz <- getTTTimeZone
  froms <- parseTimeInput tz from
  tos <- parseTimeInput tz to
  t <- getTaskByName name
  ss <- getTaskSessionsInInterval t froms tos
  let time = sum
        $ map
          (toInteger . round . fromJustMsg "timeInInterval" . sessDuration)
          ss
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
    else do
      dur <- liftIO $ sessDurationIO sess
      task <- current
      tell ["Session duration: " ++ readSeconds (round dur)]
      pure dur

reset = do
  f <- doesFileExist dbname
  when f $ removeFile dbname
