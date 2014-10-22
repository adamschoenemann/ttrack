module TTrack.CLI where

import TTrack.Commands
import TTrack.Types
import TTrack.TimeUtils
import Data.Maybe (fromJust)

handleInput :: [String] -> TrackerMonad ()
handleInput args = do
    case args of
        ["create", n] -> do
            create n
            return ()
        ["start", n] -> do
            start n
            return ()
        ["current"] -> do
            task <- current
            tell ["Current task is " ++ taskName task]
            return ()
        ["stop"] -> do
            sess <- stop
            tell ["Session duration was " ++ (readSeconds . round . fromJust $ sessDuration sess)]
            return ()
        ["list"] -> do
            list
            return ()
        ["duration"] -> do
            d <- duration
            task <- current
            tell ["Current session is with task: " ++ taskName task
                 ++ ". Session duration: " ++ (readSeconds $ round d)]
            return ()
        ["report", n] -> do
        	report n
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
        _ -> do
            tell [syntaxError]
            return ()

syntaxError :: String
syntaxError = "Usage: ttrack command\n\
              \\n\
              \commands:\n\
              \\t create {task}\t\t\t creates a new task\n\
              \\t start {task}\t\t\t starts tracking task\n\
              \\t stop \t\t\t\t stops tracking current task\n\
              \\t current \t\t\t get current task in progress\n\
              \\t duration \t\t\t print duration of current session\n\
              \\t report {task} \t\t\t print a list of sessions for the task\n\
              \\t list \t\t\t\t lists all tasks\n\
              \\t time {task} [from to] \t\t print time spent on task \n\
              \\t remove {task} \t\t\t removes task and all sessions for that task"
