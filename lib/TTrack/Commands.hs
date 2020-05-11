{-# LANGUAGE RankNTypes #-}

module TTrack.Commands where

import           Control.Monad.Except

import           Data.Char (toLower, toUpper)
import           Data.Function (on)
import           Data.List (groupBy, sort)
import           Data.List.Extras (safeHead)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Maybe.Extras (fromJustMsg)
import           Data.Ord (Down(..))
import           Data.Time

import           System.Directory (doesFileExist, removeFile)

import           TTrack.Config
import           TTrack.DB
import           TTrack.PrintTable (printTable)
import           TTrack.TimeUtils
import           TTrack.Types
import           TTrack.Utils

create :: String -> TrackerMonad Task
create name =
  do
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

instance Semigroup NominalDiffTime where
  n1 <> n2 = n1 + n2

instance Monoid NominalDiffTime where
  mempty = 0

unDown :: Down a -> a
unDown (Down x) = x

mergeSessions :: UTCTime -> [Session] -> (UTCTime, UTCTime, NominalDiffTime)
mergeSessions now ss =
  let
    dur = mconcat $ map (sessDurationWithNow now) ss
    start = fromMaybe now . safeHead . sort . map sessStart $ ss
    end =
      maybe now unDown . safeHead . sort . map (Down . fromMaybe now . sessEnd)
      $ ss
  in (start, end, dur)

tmap23 :: (forall x. x -> f x) -> (a, b, c) -> (a, f b, f c)
tmap23 f (x, y, z) = (x, f y, f z)

data ReportHeader
  = Start
  | End
  | Duration
  | Hours
  deriving (Eq, Ord, Show)

reportTable :: TimeZone -> [SessTime] -> String
reportTable tz = printTable " | " . foldr go initialMap
  where
    go st@(_, _, dur) acc =
      foldr
        (\(k, v) -> M.insertWith (++) k [v])
        acc
        [ (Start, showStart st)
        , (End, showEnd st)
        , (Duration, maybe "in progress" (readSeconds . round) dur)
        , ( Hours
            , maybe "in progress" (show . readHoursRoundQuarters . round) dur)
        ]

    initialMap = M.empty :: M.Map ReportHeader [String]

    format = "%FT%T%z"

    dtl = defaultTimeLocale

    showEnd (_, end, _) = case end of
      Nothing -> "Unended"
      Just end' -> formatTime dtl format $ utcToZonedTime tz end'

    showStart (start, _, _) = formatTime dtl format $ utcToZonedTime tz $ start

report :: String -> GroupBy -> Maybe RoundBy -> TrackerMonad ()
report n groupByOpt roundByOpt = do
  liftIO $ print roundByOpt
  -- TODO: use roundByOpt to do rounding
  task <- getTaskByName n
  sessions <- getTaskSessions task
  tz <- getTTTimeZone
  now <- getTTCurrentTime
  tell [reportTable tz $ groupIfGiven now sessions]
  where
    groupIfGiven now = case groupByOpt of
      NoGroup -> map sessToSessTime
      DayGroup -> map (tmap23 Just . mergeSessions now) . grouped
      where
        grouped = groupBy grouper

        grouper = (==) `on` utctDay . sessStart

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
  let time =
        sum
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
