{-# LANGUAGE OverloadedStrings #-}

module PyCloneSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IORef (newIORef)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Regexle.DFA (DfaInfo (..), alphabetCardinality)
import Regexle.PuzzleCache (Puzzle (..))
import Regexle.Solver
  ( Clue (..)
  , applyClueZ3PyClone
  , buildClues
  , mkAlphabetDomain
  , mkGridZ3
  , mkStateDomain
  )
import Test.Syd
import qualified Z3.Monad as Z3 (evalZ3, solverToString)

spec :: Spec
spec = describe "PyClone deduplication" $ do
  it "emits a single set of cell bans even when overlapping clues repeat them" $ do
    (clueX, clueY, _) <- buildTestClues
    let info = clueDfa clueX
        initialDead =
          let vec = diDeadFrom info
           in if V.null vec then IntSet.empty else vec V.! diInitial info
        expectedDistinct =
          IntSet.size (IntSet.union (diDeadAlphabet info) initialDead)
    alphabetRef <- newIORef IntMap.empty
    stateRef <- newIORef Map.empty
    distinctCount <- Z3.evalZ3 $ do
      let dim = puzzleDiameter testPuzzle
          stateCount = max 1 (V.length (diTransitions info))
      alphabetDomain <- mkAlphabetDomain alphabetCardinality
      stateDomain <- mkStateDomain "PyCloneTest" stateCount
      grid <- mkGridZ3 alphabetDomain dim
      applyClueZ3PyClone dim alphabetRef stateRef alphabetDomain stateDomain grid clueX
      applyClueZ3PyClone dim alphabetRef stateRef alphabetDomain stateDomain grid clueY
      solverText <- Z3.solverToString
      let isCellDistinct s = "(distinct" `List.isInfixOf` s && "cell_0_0" `List.isInfixOf` s
          linesWithDistinct = filter isCellDistinct (lines solverText)
      pure (length linesWithDistinct)
    distinctCount `shouldBe` expectedDistinct

  it "avoids re-adding identical state bans when a clue is applied twice" $ do
    (clueX, _, _) <- buildTestClues
    let info = clueDfa clueX
        stateCount = max 1 (V.length (diTransitions info))
        deadStateIdxs =
          IntSet.toList . IntSet.fromList $
            [ idx
            | idx <- IntSet.toList (diDeadStates info)
            , idx >= 0
            , idx < stateCount
            ]
        stateVarCount = length (clueCoords clueX) + 1
        expectedDistinct = stateVarCount * length deadStateIdxs
    alphabetRef <- newIORef IntMap.empty
    stateRef <- newIORef Map.empty
    distinctCount <- Z3.evalZ3 $ do
      let dim = puzzleDiameter testPuzzle
      alphabetDomain <- mkAlphabetDomain alphabetCardinality
      stateDomain <- mkStateDomain "PyCloneState" stateCount
      grid <- mkGridZ3 alphabetDomain dim
      applyClueZ3PyClone dim alphabetRef stateRef alphabetDomain stateDomain grid clueX
      applyClueZ3PyClone dim alphabetRef stateRef alphabetDomain stateDomain grid clueX
      solverText <- Z3.solverToString
      let isStateDistinct s = "(distinct" `List.isInfixOf` s && "state_x_0_" `List.isInfixOf` s
          linesWithDistinct = filter isStateDistinct (lines solverText)
      pure (length linesWithDistinct)
    distinctCount `shouldBe` expectedDistinct

buildTestClues :: IO (Clue, Clue, Clue)
buildTestClues =
  case buildClues testPuzzle of
    Left err -> expectationFailure err >> fail err
    Right (cX : cY : cZ : _) -> pure (cX, cY, cZ)
    Right _ -> expectationFailure "Expected at least three clues for test puzzle" >> fail "missing test clues"

testPuzzle :: Puzzle
testPuzzle =
  Puzzle
    { puzzleDiameter = 1
    , puzzleName = "test"
    , puzzleDay = 0
    , puzzleSide = 1
    , puzzleSeed = 0
    , puzzleCommitHash = "deadbeef"
    , puzzleX = [pattern]
    , puzzleY = [pattern]
    , puzzleZ = [pattern]
    }
  where
    pattern :: T.Text
    pattern = "A"
