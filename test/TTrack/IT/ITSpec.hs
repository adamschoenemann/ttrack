
module TTrack.IT.ITSpec where

import           Data.Time.Clock
import           TTrack.Types
import           TTrack.Commands
import           Test.Hspec
import           Test.QuickCheck
import           Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
  describe "starting a session" $
    pure ()

  describe "sessDurationWithNow" $
    it "equals sessDuration if sessEnd != Nothing" $ do
      let task = Task { taskId = 0, taskName = "Some task" }
      now <- liftIO getCurrentTime
      let start1 = now
      let dur1 = 60 * 60 * 3
      let end1 = addUTCTime dur1 start1
      let s1 = Session
            { sessId = 0
            , sessTask = task
            , sessStart = now
            , sessEnd = Just end1
            }
      sessDuration s1 `shouldBe` Just dur1
      sessDurationWithNow (UTCTime (utctDay now) 0) s1 `shouldBe` dur1

  describe "merging sessions works" $
    it "adds the duration of all the merged sessions" $ do
      let task = Task { taskId = 0, taskName = "Some task" }
      now <- liftIO getCurrentTime
      let dur1 = 60 * 60 * 3
      let dur2 = 60 * 60 * 2
      let break = 60 * 60
      let start1 = now
      let end1 = addUTCTime dur1 start1
      let start2 = addUTCTime break end1
      let end2 = addUTCTime dur2 start2
      let s1 = Session
            { sessId = 0
            , sessTask = task
            , sessStart = start1
            , sessEnd = Just end1
            }
      let s2 = Session
            { sessId = 0
            , sessTask = task
            , sessStart = start2
            , sessEnd = Just end2
            }
      let (start, end, duration) = mergeSessions now [s1, s2]
      start `shouldBe` start1
      end `shouldBe` end2
      duration `shouldBe` (dur1 + dur2)
