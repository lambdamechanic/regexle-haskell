#!/usr/bin/env runghc
import Control.Monad (forM, forM_)
import qualified Data.IntSet as IntSet
import qualified Data.Text as T
import qualified Data.Vector as V
import Regexle.PuzzleCache
import Regexle.RegexParser
import Regexle.DFA

main :: IO ()
main = do
  let side = 3
      days = [400 .. 409]
  puzzles <- mapM (`fetchPuzzle` side) days
  forM_ puzzles $ \puzzle -> do
    let pats = puzzleX puzzle ++ puzzleY puzzle ++ puzzleZ puzzle
    stats <- forM pats $ \pat -> do
      case parseRegexToERE pat of
        Left err -> fail $ "Parse error on day " ++ show (puzzleDay puzzle) ++ ": " ++ err
        Right ere -> do
          let info = fromERE ere
              deadAlphabet = diDeadAlphabet info
              transitions = diTransitions info
              len = V.length transitions
              banned = IntSet.size deadAlphabet
          pure (len, banned)
    let avgStates = average (map fst stats)
        avgBanned = average (map snd stats)
        maxBanned = maximum (map snd stats)
    putStrLn $ "Day " ++ show (puzzleDay puzzle)
      ++ ": avgStates=" ++ show avgStates
      ++ " avgBanned=" ++ show avgBanned
      ++ " maxBanned=" ++ show maxBanned

average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)
