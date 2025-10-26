{-# LANGUAGE OverloadedStrings #-}

module Regexle.Solver
  ( SolveBackend (..)
  , SolveConfig (..)
  , SolveResult (..)
  , TransitionEncoding (..)
  , defaultSolveConfig
  , buildClues
  , solvePuzzle
  , solvePuzzleWith
  , solvePuzzleWithClues
  , solvePuzzlesHot
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (foldM, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.IntSet as IntSet
import Data.IORef (newIORef, readIORef, writeIORef)
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
import qualified Z3.Monad as Z3
import Text.Read (readMaybe)

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

-------------------------------------------------------------------------------
-- Types and configuration
-------------------------------------------------------------------------------

data TransitionEncoding = UseLookup | UseLambda | UseEnum
  deriving (Eq, Show, Enum, Bounded)

data SolveBackend
  = BackendSBV
  | BackendZ3Direct
  deriving (Eq, Show)

data SolveConfig = SolveConfig
  { scBackend :: !SolveBackend
  , scTransitionEncoding :: !TransitionEncoding
  }
  deriving (Eq, Show)

defaultSolveConfig :: SolveConfig
defaultSolveConfig =
  SolveConfig
    { scBackend = BackendSBV
    , scTransitionEncoding = UseLambda
    }

-------------------------------------------------------------------------------
-- Solver-facing types
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

solvePuzzle :: Puzzle -> IO (Either String SolveResult)
solvePuzzle = solvePuzzleWith defaultSolveConfig

solvePuzzleWith :: SolveConfig -> Puzzle -> IO (Either String SolveResult)
solvePuzzleWith cfg puzzle =
  case buildClues puzzle of
    Left err -> pure (Left err)
    Right clues -> solvePuzzleWithClues cfg puzzle clues

solvePuzzleWithClues :: SolveConfig -> Puzzle -> [Clue] -> IO (Either String SolveResult)
solvePuzzleWithClues cfg puzzle clues =
  case scBackend cfg of
    BackendSBV -> solvePuzzleSBV (scTransitionEncoding cfg) puzzle clues
    BackendZ3Direct -> solvePuzzleZ3Direct puzzle clues

solvePuzzlesHot :: SolveConfig -> [(Puzzle, [Clue])] -> IO [Either String SolveResult]
solvePuzzlesHot cfg puzzlesWithClues =
  case scBackend cfg of
    BackendSBV -> mapM (uncurry (solvePuzzleSBV (scTransitionEncoding cfg))) puzzlesWithClues
    BackendZ3Direct -> mapM (uncurry solvePuzzleZ3Direct) puzzlesWithClues

-------------------------------------------------------------------------------
-- SBV backend (existing implementation)
-------------------------------------------------------------------------------

solvePuzzleSBV :: TransitionEncoding -> Puzzle -> [Clue] -> IO (Either String SolveResult)
solvePuzzleSBV _encoding puzzle clues = do
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

-------------------------------------------------------------------------------
-- Direct Z3 backend
-------------------------------------------------------------------------------

data AlphabetDomain = AlphabetDomain
  { adSort :: !Z3.Sort
  , adValues :: !(V.Vector Z3.AST)
  }

data StateDomain = StateDomain
  { sdSort :: !Z3.Sort
  , sdValues :: !(V.Vector Z3.AST)
  }

data TransitionFunction = TransitionFunction
  { tfParamState :: !Z3.AST
  , tfParamChar :: !Z3.AST
  , tfExpr :: !Z3.AST
  }

mkAlphabetDomain :: Int -> Z3.Z3 AlphabetDomain
mkAlphabetDomain n = do
  sym <- Z3.mkStringSymbol "AlphabetSort"
  let size = max 1 n
  sort <- Z3.mkFiniteDomainSort sym (fromIntegral size)
  values <- V.generateM size $ \i -> Z3.mkInt i sort
  pure AlphabetDomain { adSort = sort, adValues = values }

mkStateDomain :: String -> Int -> Z3.Z3 StateDomain
mkStateDomain label n = do
  sym <- Z3.mkStringSymbol ("StateSort_" ++ label)
  let size = max 1 n
  sort <- Z3.mkFiniteDomainSort sym (fromIntegral size)
  values <- V.generateM size $ \i -> Z3.mkInt i sort
  pure StateDomain { sdSort = sort, sdValues = values }

alphabetLiteral :: AlphabetDomain -> Int -> Z3.AST
alphabetLiteral dom idx = adValues dom V.! idx

stateLiteral :: StateDomain -> Int -> Z3.AST
stateLiteral dom idx = sdValues dom V.! idx

solvePuzzleZ3Direct :: Puzzle -> [Clue] -> IO (Either String SolveResult)
solvePuzzleZ3Direct puzzle clues = do
  buildStart <- getCurrentTime
  stageRef <- newIORef "initializing"
  let sideVal = puzzleSide puzzle
  resultOrErr <- (try $ Z3.evalZ3 $ do
    alphabetDomain <- mkAlphabetDomain alphabetCardinality
    liftIO (writeIORef stageRef "mkGrid")
    grid <- mkGridZ3 alphabetDomain (puzzleDiameter puzzle)
    forM_ clues $ \clue -> do
      liftIO (writeIORef stageRef ("clue " ++ clueLabel clue))
      applyClueZ3 alphabetDomain grid clue
    liftIO (writeIORef stageRef "solverCheck")
    solveStart <- liftIO getCurrentTime
    (res, mModel) <- Z3.solverCheckAndGetModel
    solveEnd <- liftIO getCurrentTime
    case (res, mModel) of
      (Z3.Sat, Just model) -> do
        liftIO (writeIORef stageRef "extractModel")
        values <- forM (zip [0 :: Int ..] grid) $ \(xIdx, row) ->
          forM (zip [0 :: Int ..] row) $ \(yIdx, cell) -> do
            liftIO (writeIORef stageRef ("extractModel cell " ++ show (xIdx, yIdx)))
            if abs (xIdx - yIdx) >= sideVal
              then pure 0
              else extractCellValue model cell
        pure (Just values, solveStart, solveEnd)
      _ -> pure (Nothing, solveStart, solveEnd)
    ) :: IO (Either SomeException (Maybe [[Word8]], UTCTime, UTCTime))
  case resultOrErr of
    Left ex -> do
      stage <- readIORef stageRef
      pure (Left ("Z3 solver failed during " ++ stage ++ ": " ++ displayException ex))
    Right (Nothing, _, _) -> pure (Left "Z3 solver reported UNSAT/UNKNOWN")
    Right (Just modelVals, solveStart, solveEnd) -> do
      let buildTime = toSeconds (diffUTCTime solveStart buildStart)
          solveTime = toSeconds (diffUTCTime solveEnd solveStart)
          rendered = renderGrid puzzle modelVals
      pure (Right SolveResult {srGrid = rendered, srBuildTime = buildTime, srSolveTime = solveTime})
  where
    toSeconds :: NominalDiffTime -> Double
    toSeconds = realToFrac

mkGridZ3 :: AlphabetDomain -> Int -> Z3.Z3 [[Z3.AST]]
mkGridZ3 alphabetDomain dim =
  forM [0 .. dim - 1] $ \x ->
    forM [0 .. dim - 1] $ \y ->
      Z3.mkFreshConst ("cell_" ++ show x ++ "_" ++ show y) (adSort alphabetDomain)

applyClueZ3 :: AlphabetDomain -> [[Z3.AST]] -> Clue -> Z3.Z3 ()
applyClueZ3 alphabetDomain grid clue = do
  let chars = map (\(x, y) -> grid !! x !! y) (clueCoords clue)
      dfaInfo = clueDfa clue
      transitions = diTransitions dfaInfo
      stateCount = max 1 (V.length transitions)
  stateDomain <- mkStateDomain (clueLabel clue) stateCount
  transitionFn <- buildTransitionFunction stateDomain alphabetDomain dfaInfo
  states <- mkStateVarsZ3 stateDomain (length chars) (clueAxis clue) (clueIndex clue)
  restrictStatesZ3 stateDomain dfaInfo states
  mapM_ (restrictDeadAlphabetZ3 alphabetDomain dfaInfo) chars
  case (states, chars) of
    (st0 : _, firstChar : _) -> do
      restrictInitialZ3 alphabetDomain dfaInfo firstChar
      let initLit = stateLiteral stateDomain (diInitial dfaInfo)
      eq <- Z3.mkEq st0 initLit
      Z3.assert eq
    (st0 : _, []) -> do
      let initLit = stateLiteral stateDomain (diInitial dfaInfo)
      eq <- Z3.mkEq st0 initLit
      Z3.assert eq
    _ -> pure ()
  applyTransitionsZ3 transitionFn states chars
  case lastMaybe states of
    Just finalState -> assertAcceptStateZ3 stateDomain dfaInfo finalState
    Nothing -> pure ()

mkStateVarsZ3 :: StateDomain -> Int -> Char -> Int -> Z3.Z3 [Z3.AST]
mkStateVarsZ3 stateDomain len axis idx =
  forM [0 .. len] $ \i ->
    Z3.mkFreshConst ("state_" ++ [axis] ++ "_" ++ show idx ++ "_" ++ show i) (sdSort stateDomain)

restrictStatesZ3 :: StateDomain -> DfaInfo -> [Z3.AST] -> Z3.Z3 ()
restrictStatesZ3 stateDomain info states = do
  let deadList = IntSet.toList (diDeadStates info)
  forM_ states $ \st ->
    forM_ deadList $ \d -> do
      let deadLit = stateLiteral stateDomain d
      eq <- Z3.mkEq st deadLit
      neq <- Z3.mkNot eq
      Z3.assert neq

restrictInitialZ3 :: AlphabetDomain -> DfaInfo -> Z3.AST -> Z3.Z3 ()
restrictInitialZ3 alphabetDomain info firstChar = do
  let deadFrom = diDeadFrom info
      initialDead = if V.null deadFrom then IntSet.empty else deadFrom V.! diInitial info
  forM_ (IntSet.toList initialDead) $ \idx -> do
    let idxLit = alphabetLiteral alphabetDomain idx
    eq <- Z3.mkEq firstChar idxLit
    neq <- Z3.mkNot eq
    Z3.assert neq

restrictDeadAlphabetZ3 :: AlphabetDomain -> DfaInfo -> Z3.AST -> Z3.Z3 ()
restrictDeadAlphabetZ3 alphabetDomain info cell =
  forM_ (IntSet.toList (diDeadAlphabet info)) $ \idx -> do
    let idxLit = alphabetLiteral alphabetDomain idx
    eq <- Z3.mkEq cell idxLit
    neq <- Z3.mkNot eq
    Z3.assert neq

applyTransitionsZ3 :: TransitionFunction -> [Z3.AST] -> [Z3.AST] -> Z3.Z3 ()
applyTransitionsZ3 _ _ [] = pure ()
applyTransitionsZ3 _ [_] _ = pure ()
applyTransitionsZ3 transFn (prev:next:restStates) (ch:chs) = do
  expected <- applyTransitionFunction transFn prev ch
  eq <- Z3.mkEq next expected
  Z3.assert eq
  applyTransitionsZ3 transFn (next:restStates) chs
applyTransitionsZ3 _ _ _ = pure ()

assertAcceptStateZ3 :: StateDomain -> DfaInfo -> Z3.AST -> Z3.Z3 ()
assertAcceptStateZ3 stateDomain info finalState =
  case IntSet.toList (diAccepting info) of
    [] -> do
      falseNode <- Z3.mkFalse
      Z3.assert falseNode
    xs -> do
      comparisons <- forM xs $ \v -> do
        let lit = stateLiteral stateDomain v
        Z3.mkEq finalState lit
      cond <- Z3.mkOr comparisons
      Z3.assert cond

transitionLookupZ3 :: StateDomain -> AlphabetDomain -> DfaInfo -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST
transitionLookupZ3 stateDomain alphabetDomain info st ch = do
  let rows = zip [0 :: Int ..] (V.toList (diTransitions info))
  let defaultLit = stateLiteral stateDomain 0
  foldM step defaultLit rows
  where
    step acc (idx, row) = do
      let idxLit = stateLiteral stateDomain idx
      cond <- Z3.mkEq st idxLit
      rowVal <- lookupRow row
      Z3.mkIte cond rowVal acc
    lookupRow row = do
      let cols = zip [0 :: Int ..] (V.toList row)
          defaultDest = if V.null row then 0 else V.last row
      let defaultDestLit = stateLiteral stateDomain defaultDest
      foldM (stepCol ch) defaultDestLit cols
    stepCol cell acc (cIdx, dst) = do
      let charLit = alphabetLiteral alphabetDomain cIdx
          dstLit = stateLiteral stateDomain dst
      cond <- Z3.mkEq cell charLit
      Z3.mkIte cond dstLit acc

buildTransitionFunction :: StateDomain -> AlphabetDomain -> DfaInfo -> Z3.Z3 TransitionFunction
buildTransitionFunction stateDomain alphabetDomain info = do
  stateParam <- Z3.mkFreshConst "delta_state" (sdSort stateDomain)
  charParam <- Z3.mkFreshConst "delta_char" (adSort alphabetDomain)
  expr <- transitionLookupZ3 stateDomain alphabetDomain info stateParam charParam
  pure TransitionFunction
    { tfParamState = stateParam
    , tfParamChar = charParam
    , tfExpr = expr
    }

applyTransitionFunction :: TransitionFunction -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST
applyTransitionFunction TransitionFunction { tfParamState = stateParam
                                           , tfParamChar = charParam
                                           , tfExpr = expr
                                           } st ch =
  Z3.substitute expr [(stateParam, st), (charParam, ch)]

clueLabel :: Clue -> String
clueLabel clue =
  let patternPreview = take 24 (T.unpack (cluePattern clue))
      prefix = [clueAxis clue] ++ show (clueIndex clue)
   in prefix ++ if null patternPreview then "" else " " ++ patternPreview

extractCellValue :: Z3.Model -> Z3.AST -> Z3.Z3 Word8
extractCellValue model cell = do
  mVal <- Z3.modelEval model cell True
  case mVal of
    Nothing -> liftIO (ioError (userError "Z3 model missing cell value"))
    Just ast -> do
      numStr <- Z3.getNumeralString ast
      case readMaybe numStr of
        Just n | n >= 0 && n <= 255 -> pure (fromIntegral (n :: Integer))
        _ -> do
          rendered <- Z3.astToString ast
          liftIO (ioError (userError ("Unexpected Z3 numeral: " ++ rendered ++ " (" ++ numStr ++ ")")))

-------------------------------------------------------------------------------
-- Shared helpers
-------------------------------------------------------------------------------

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
