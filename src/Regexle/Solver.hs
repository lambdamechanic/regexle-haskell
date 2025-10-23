{-# LANGUAGE OverloadedStrings #-}

module Regexle.Solver
  ( SolveResult (..)
  , solvePuzzle
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, forM_)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import Data.Word (Word8, Word16)
import Data.SBV
  ( SWord16
  , SWord8
  , Symbolic
  , constrain
  , literal
  , runSMTWith
  , sWord16
  , sWord8
  , (.==)
  , (./=)
  , (.<=)
  , (.||)
  )
import qualified Data.SBV as SBV
import Data.SBV.Control
  ( CheckSatResult (Sat, Unsat)
  , checkSat
  , getValue
  , io
  , query
  )
import Regexle.DFA
  ( DfaInfo (..)
  , alphabetCardinality
  , fromERE
  , indexToChar
  )
import Regexle.PuzzleCache
  ( Puzzle (..)
  )
import Regexle.RegexParser (parseRegexToERE)

data Clue = Clue
  { clueAxis :: !Char
  , clueIndex :: !Int
  , cluePattern :: !Text
  , clueDfa :: !DfaInfo
  , clueCoords :: ![(Int, Int)]
  }

data SolveResult = SolveResult
  { srGrid :: [[Char]]
  , srBuildTime :: !Double
  , srSolveTime :: !Double
  }
  deriving (Eq, Show)

solvePuzzle :: Puzzle -> IO (Either String SolveResult)
solvePuzzle puzzle = do
  case buildClues puzzle of
    Left err -> pure (Left err)
    Right clues -> do
      buildStart <- getCurrentTime
      resultOrErr <- (try $ runSMTWith SBV.z3{SBV.verbose = False} $ do
        grid <- mkGrid (puzzleDiameter puzzle)
        mapM_ constrainCellDomain (concat grid)
        mapM_ (applyClue grid) clues
        query $ do
          solveStart <- io getCurrentTime
          cs <- checkSat
          solveEnd <- io getCurrentTime
          case cs of
            Sat -> do
              model <- mapM (mapM getValue) grid
              pure (Just model, solveStart, solveEnd)
            Unsat -> pure (Nothing, solveStart, solveEnd)
            _ -> pure (Nothing, solveStart, solveEnd)) :: IO (Either SomeException (Maybe [[Word8]], UTCTime, UTCTime))

      case resultOrErr of
        Left ex ->
          pure (Left ("SBV solver failed: " ++ displayException ex))
        Right (Nothing, _, _) ->
          pure (Left "SBV solver reported UNSAT")
        Right (Just model, solveStart, solveEnd) -> do
          let buildTime = toSeconds (diffUTCTime solveStart buildStart)
              solveTime = toSeconds (diffUTCTime solveEnd solveStart)
              rendered = renderGrid puzzle model
          pure (Right SolveResult {srGrid = rendered, srBuildTime = buildTime, srSolveTime = solveTime})
  where
    toSeconds :: NominalDiffTime -> Double
    toSeconds = realToFrac

mkGrid :: Int -> Symbolic [[SWord8]]
mkGrid dim =
  forM [0 .. dim - 1] $ \x ->
    forM [0 .. dim - 1] $ \y ->
      sWord8 ("cell_" ++ show x ++ "_" ++ show y)

constrainCellDomain :: SWord8 -> Symbolic ()
constrainCellDomain cell =
  let maxAlphabet = literal (fromIntegral (alphabetCardinality - 1) :: Word8)
   in constrain (cell .<= maxAlphabet)

applyClue :: [[SWord8]] -> Clue -> Symbolic ()
applyClue grid clue = do
  let chars = map (\(x, y) -> grid !! x !! y) (clueCoords clue)
      dfaInfo = clueDfa clue
  states <- mkStateVars (length chars) (clueAxis clue) (clueIndex clue)
  restrictStates dfaInfo states
  mapM_ (restrictDeadAlphabet dfaInfo) chars
  case (states, chars) of
    (st0 : _, firstChar : _) -> do
      restrictInitial dfaInfo firstChar
      constrain (st0 .== literalState (diInitial dfaInfo))
    (st0 : _, []) ->
      constrain (st0 .== literalState (diInitial dfaInfo))
    _ -> pure ()
  applyTransitions dfaInfo states chars
  case lastMaybe states of
    Just finalState -> constrain (acceptConstraint dfaInfo finalState)
    Nothing -> pure ()

mkStateVars :: Int -> Char -> Int -> Symbolic [SWord16]
mkStateVars len axis idx =
  forM [0 .. len] $ \i ->
    sWord16 ("state_" ++ [axis] ++ "_" ++ show idx ++ "_" ++ show i)

restrictStates :: DfaInfo -> [SWord16] -> Symbolic ()
restrictStates info states = do
  let transitions = diTransitions info
      maxState =
        if V.null transitions
          then literalState 0
          else literalState (V.length transitions - 1)
      deadList = IntSet.toList (diDeadStates info)
  forM_ states $ \st -> do
    constrain (st .<= maxState)
    forM_ deadList $ \d ->
      constrain (st ./= literalState d)

restrictInitial :: DfaInfo -> SWord8 -> Symbolic ()
restrictInitial info firstChar = do
  let deadFrom = diDeadFrom info
      initialDead = if V.null deadFrom then IntSet.empty else deadFrom V.! diInitial info
  forM_ (IntSet.toList initialDead) $ \idx ->
    constrain (firstChar ./= literal (fromIntegral idx :: Word8))

restrictDeadAlphabet :: DfaInfo -> SWord8 -> Symbolic ()
restrictDeadAlphabet info cell =
  forM_ (IntSet.toList (diDeadAlphabet info)) $ \idx ->
    constrain (cell ./= literal (fromIntegral idx :: Word8))

applyTransitionConstraint :: DfaInfo -> SWord16 -> (SWord8, SWord16) -> Symbolic ()
applyTransitionConstraint info statePrev (ch, stateNext) = do
  let expected = transitionLookup info statePrev ch
  constrain (stateNext .== expected)

applyTransitions :: DfaInfo -> [SWord16] -> [SWord8] -> Symbolic ()
applyTransitions _ _ [] = pure ()
applyTransitions _ [_] _ = pure ()
applyTransitions info (prev:next:restStates) (ch:chs) = do
  applyTransitionConstraint info prev (ch, next)
  applyTransitions info (next:restStates) chs
applyTransitions _ _ _ = pure ()

acceptConstraint :: DfaInfo -> SWord16 -> SBV.SBool
acceptConstraint info finalState =
  case IntSet.toList (diAccepting info) of
    [] -> SBV.literal False
    xs ->
      foldl'
        (\acc v -> acc .|| (finalState .== literalState v))
        (SBV.literal False)
        xs

transitionLookup :: DfaInfo -> SWord16 -> SWord8 -> SWord16
transitionLookup info st ch =
  foldr
    (\(idx, row) acc ->
        SBV.ite (st .== literalState idx)
          (lookupRow row)
          acc)
    (literalState 0)
    (zip [0 :: Int ..] (V.toList (diTransitions info)))
  where
    lookupRow row =
      let defaultDest =
            if V.null row then 0 else V.last row
       in foldr
            (\(cIdx, dst) acc ->
                SBV.ite (ch .== literal (fromIntegral cIdx :: Word8))
                  (literalState dst)
                  acc)
            (literalState defaultDest)
            (zip [0 :: Int ..] (V.toList row))

literalState :: Int -> SWord16
literalState n = literal (fromIntegral n :: Word16)

renderGrid :: Puzzle -> [[Word8]] -> [[Char]]
renderGrid puzzle values =
  [ [ renderCell x y (values !! x !! y)
    | y <- [0 .. dim - 1]
    ]
  | x <- [0 .. dim - 1]
  ]
  where
    dim = puzzleDiameter puzzle
    sideVal = puzzleSide puzzle
    renderCell x y v
      | abs (x - y) >= sideVal = ' '
      | otherwise =
          fromMaybe '?' (indexToChar (fromIntegral v))

buildClues :: Puzzle -> Either String [Clue]
buildClues puzzle = do
  let side = puzzleSide puzzle
      dim = puzzleDiameter puzzle
  cluesX <- traverse (buildAxis 'x' (axisX dim side)) (zip [0 ..] (puzzleX puzzle))
  cluesY <- traverse (buildAxis 'y' (axisY dim side)) (zip [0 ..] (puzzleY puzzle))
  cluesZ <- traverse (buildAxis 'z' (axisZ dim side)) (zip [0 ..] (puzzleZ puzzle))
  pure (cluesX ++ cluesY ++ cluesZ)
  where
    buildAxis axis coordFn (idx, pat) = do
      ere <- case parseRegexToERE pat of
        Left err -> Left (formatError axis idx pat err)
        Right val -> Right val
      let dfa = fromERE ere
          coords = coordFn idx
      pure Clue
        { clueAxis = axis
        , clueIndex = idx
        , cluePattern = pat
        , clueDfa = dfa
        , clueCoords = coords
        }

    formatError axis idx pat err =
      "Pattern parse error for "
        ++ [axis]
        ++ show idx
        ++ ": "
        ++ err
        ++ " (pattern: "
        ++ T.unpack pat
        ++ ")"

axisX :: Int -> Int -> Int -> [(Int, Int)]
axisX dim side x =
  [ (x, y)
  | y <- reverse [0 .. dim - 1]
  , abs (x - y) < side
  ]

axisY :: Int -> Int -> Int -> [(Int, Int)]
axisY dim side y =
  [ (x, y)
  | x <- [0 .. dim - 1]
  , abs (x - y) < side
  ]

axisZ :: Int -> Int -> Int -> [(Int, Int)]
axisZ dim side z =
  [ (x, y)
  | (x, y) <- zip [0 .. dim - 1] (map (\n -> side - 1 - z + n) [0 .. dim - 1])
  , x >= 0, x < dim
  , y >= 0, y < dim
  , abs (x - y) < side
  ]

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x:xs) = Just (go x xs)
  where
    go current [] = current
    go _ (y:ys) = go y ys
