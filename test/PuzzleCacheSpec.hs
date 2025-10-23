module PuzzleCacheSpec (spec) where

import Control.Exception (bracket_)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Regexle.PuzzleCache
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd

samplePuzzle :: Puzzle
samplePuzzle =
  Puzzle
    { puzzleDiameter = 5
    , puzzleName = T.pack "\"Day: 123 Side: 3\""
    , puzzleDay = 123
    , puzzleSide = 3
    , puzzleSeed = 4242
    , puzzleCommitHash = T.pack "deadbeef"
    , puzzleX = map T.pack ["(AB|C)+"]
    , puzzleY = map T.pack ["(D|E)+"]
    , puzzleZ = map T.pack ["(F|G)+"]
    }

spec :: Spec
spec = do
  describe "puzzleCachePath" $
    it "honors XDG cache home" $
      withSystemTempDirectory "regexle-cache" $ \dir ->
        withXdgCacheHome dir $ do
          path <- puzzleCachePath 1 2
          path `shouldBe` dir </> "regexle" </> "puzzle_1_2.json"

  describe "fetchPuzzle" $
    it "reads puzzle from existing cache without network" $
      withSystemTempDirectory "regexle-cache" $ \dir ->
        withXdgCacheHome dir $ do
          path <- puzzleCachePath (puzzleDay samplePuzzle) (puzzleSide samplePuzzle)
          createDirectoryIfMissing True (takeDirectory path)
          BL.writeFile path (encode samplePuzzle)

          result <- fetchPuzzle (puzzleDay samplePuzzle) (puzzleSide samplePuzzle)
          result `shouldBe` samplePuzzle

withXdgCacheHome :: FilePath -> IO a -> IO a
withXdgCacheHome dir action = do
  old <- lookupEnv "XDG_CACHE_HOME"
  bracket_ (setEnv "XDG_CACHE_HOME" dir) (restore old) action
  where
    restore Nothing = unsetEnv "XDG_CACHE_HOME"
    restore (Just v) = setEnv "XDG_CACHE_HOME" v
