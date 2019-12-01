module TTrack.CLI where

import           Data.Maybe.Extras (fromJustMsg)

import           TTrack.Commands
import           TTrack.TimeUtils
import           TTrack.Types

handleInput :: [String] -> TrackerMonad ()
handleInput args = case args of
  ["create", n] -> do
    create n
    return ()
  ["start", n] -> do
    start n Nothing
    return ()
  ["start", n, b] -> do
    start n (Just b)
    return ()
  ["cancel"] -> do
    cancel
    return ()
  ["purge", n] -> do
    purge n
    return ()
  ["current"] -> do
    task <- current
    tell ["Current task is " ++ taskName task]
    return ()
  ["stop"] -> do
    sess <- stop
    tell
      [ "Session duration was "
          ++ (readSeconds . round . fromJustMsg "sessDuration sess"
              $ sessDuration sess)
      ]
    return ()
  ["list"] -> do
    list
    return ()
  ["duration"] -> do
    d <- duration
    task <- current
    tell
      [ "Current session is with task: "
          ++ taskName task
          ++ ". Session duration: "
          ++ (readSeconds $ round d)
      ]
    return ()
  ["report", n] -> report n
  ["remove", n] -> do
    remove n
    return ()
  ["time", n] -> do
    t <- time n
    tell $ ["Time spent on task " ++ n ++ " is " ++ (readSeconds $ round t)]
    return ()
  ["time", n, from, to] -> do
    time <- timeInInterval n from to
    tell ["Time spent on task: " ++ n ++ ": " ++ readSeconds (round time)]
    return ()
  _ -> tell [syntaxError] >> pure ()

syntaxError :: String
syntaxError = "Usage: ttrack command\n\
              \\n\
              \commands:\n\
              \\tcreate {task}\t\t\t creates a new task\n\
              \\tstart {task} [begin] \t\t starts tracking task from now or [begin : Date]\n\
              \\tstop \t\t\t\t stops tracking current task\n\
              \\tcurrent \t\t\t get current task in progress\n\
              \\tduration \t\t\t print duration of current session\n\
              \\treport {task} \t\t\t print a list of sessions for the task\n\
              \\tlist \t\t\t\t lists all tasks\n\
              \\ttime {task} [from to] \t\t print time spent on task \n\
              \\tremove {task} \t\t\t removes task and all sessions for that task\n\
              \\tcancel \t\t\t\t cancels the current task, if any\n\
              \\tpurge {task} \t\t\t deletes all unended sessions for the given task\n\
              \formats:\n\
              \\tDates are parsed as ISO formatted date strings.\n\
              \\tYou can leave out date prefixes and timezones are optional, e.g\n\
              \\thh:mm is the minimal date to give, which will default to today's date\n\
              \\tand 0 seconds in your OS' timezone."

