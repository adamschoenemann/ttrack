module TTrack.CLI where

import           Data.Functor (($>))
import           Data.Maybe (fromMaybe)
import           Data.Maybe.Extras (fromJustMsg)
import           Data.Semigroup

import           Database.HDBC
import           Database.HDBC.Sqlite3

import           Options.Applicative

import           TTrack.Commands
import           TTrack.TimeUtils
import           TTrack.Types

strarg :: Parser String
strarg = argument str idm

idminfo :: Parser a -> ParserInfo a
idminfo p = info p idm

unitA :: Applicative m => (a -> m b) -> a -> m ()
unitA f x = f x $> ()

cli = hsubparser
  $ mconcat
    [ createCmd
    , startCmd
    , purgeCmd
    , cancelCmd
    , currentCmd
    , stopCmd
    , listCmd
    , durationCmd
    , reportCmd
    , removeCmd
    , timeCmd
    ]
  where
    createCmd = cmd "create" "Create a new task" (unitA create <$> strarg)

    startCmd = cmd
      "start"
      "Start tracking a task"
      ((\n b -> start n b $> ()) <$> strarg <*> optional strarg)

    cancelCmd =
      cmd "cancel" "Cancel the current task, if any" (pure $ cancel $> ())

    purgeCmd = cmd
      "purge"
      "Delete all undended sessions for the given task"
      (unitA purge <$> strarg)

    currentCmd =
      cmd "current" "Get the current task in progress" (pure $ current $> ())

    stopCmd = cmd "stop" "Stop tracking the current task" (pure $ stop $> ())

    listCmd = cmd "list" "List all tasks" (pure $ list $> ())

    durationCmd = cmd
      "duration"
      "Print duration of the current session"
      (pure $ duration $> ())

    reportCmd = cmd
      "report"
      "Print a list of sessions for a task"
      (unitA report <$> strarg)

    removeCmd = cmd
      "remove"
      "Remove a task and all its sessions"
      (unitA remove <$> strarg)

    timeCmd = cmd
      "time"
      "Print time spent on a task"
      (performTimeCmd <$> strarg <*> timeOpt "from" 'f' <*> timeOpt "to" 't')

    timeOpt lname sname = optional
      $ strOption (long lname <> short sname <> metavar "DATE")
      :: Parser (Maybe String)

    performTimeCmd n mfrom mto = do
      t <- fromMaybe (time n) (liftA2 (timeInInterval n) mfrom mto)
      tell ["Time spent on task: " ++ n ++ ": " ++ readSeconds (round t)]
      pure ()

    cmd c desc p = command c (info p (progDesc desc))

parseCli = execParser
  (info (cli <**> helper) (footer "Give COMMAND --help for more info"))
