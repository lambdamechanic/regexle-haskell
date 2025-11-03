#!/usr/bin/env runghc

import Control.Monad (forM, forM_)
import Data.List (foldl')
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
  putStrLn "Day state statistics (state counts per clue)"
  perPuzzle <- forM puzzles $ \puzzle -> do
    let pats = puzzleX puzzle ++ puzzleY puzzle ++ puzzleZ puzzle
    counts <- forM pats $ \pat ->
      case parseRegexToERE pat of
        Left err -> fail $ "Failed to parse pattern on day " ++ show (puzzleDay puzzle) ++ ": " ++ err
        Right ere -> do
          let info = fromERE ere
          pure (V.length (diTransitions info))
    let total = sum counts
        maxCount = maximum counts
        minCount = minimum counts
        bigClues = length (filter (> 128) counts)
        avgCount = fromIntegral total / fromIntegral (length counts)
    putStrLn $ "Day " ++ show (puzzleDay puzzle)
      ++ ": min=" ++ show minCount
      ++ " max=" ++ show maxCount
      ++ " avg=" ++ show avgCount
      ++ " big(>128)=" ++ show bigClues
    pure (puzzleDay puzzle, counts)
  putStrLn "\nChunk maxima (size 5)"
  let chunkSize = 5
      chunks = chunk chunkSize perPuzzle
  forM_ (zip [1 :: Int ..] chunks) $ \(idx, chunkEntries) -> do
    let joinedCounts = concatMap snd chunkEntries
        maxStates = if null joinedCounts then 0 else maximum joinedCounts
    putStrLn $ "Chunk " ++ show idx ++ " maxStates=" ++ show maxStates

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs
  | n <= 0 = [xs]
  | otherwise = take n xs : chunk n (drop n xs)
