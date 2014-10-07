# About
This is a simple command-line time-tracker written in Haskell.
It is designed to be simple and manual, making no assumptions.
You issue a command before you start your task, and when you are done, you issue a command to end it. If there are inconsistencies with the time you actually spent, you can correct the report, allowing complete control.

# Usage
Usage: ttrack command

commands:
         create [task]                   creates a new task
         start [task]                    starts tracking task
         end                             stops tracking current task
         current                         get current task in progress
         duration                        print duration of current session
         report [task]                   print a list of sessions for the task
         list                            lists all tasks
         time [task]                     print time spent on task
         remove [task]                   removes task and all sessions for that task

# Install
- clone the repository
- Issue commands
````
	cabal config
	cabal build
	# optionally
	cabal install
````
The generated exectuable is called `ttrack`.
`ttrack` creates a SQLite database file to keep track of your time. You can specify the location using the environment variable `TTRACK_DIR`. By default, it uses your default app directory (e.g. C:\Users\user-name\AppData\Roaming on Windows).