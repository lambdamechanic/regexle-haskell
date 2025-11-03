{-# LANGUAGE OverloadedStrings #-}

module Regexle.Solver
  ( SolveBackend (..)
  , SolveConfig (..)
  , SolveResult (..)
  , Clue (..)
  , Z3TransitionEncoding (..)
  , defaultSolveConfig
  , buildClues
  , buildCluesCached
  , solvePuzzle
  , solvePuzzleWith
  , solvePuzzleWithClues
  , solvePuzzlesHot
  , solvePuzzlesZ3DirectHot
  , solvePuzzlesZ3DirectHotWithLimit
  , solvePuzzlesZ3DirectHotNoChunk
  , solvePuzzlesZ3DirectHotWithDump
  , HotDumpSpec (..)
  , hotDumpBaseFile
  , hotDumpPuzzleFile
  , enableZ3ConsistencyChecks
  , mkAlphabetDomain
  , mkStateDomain
  , mkGridZ3
  , applyClueZ3PyClone
  ) where

import Control.Exception (SomeException, displayException, finally, try)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified Z3.Monad as Z3

import Regexle.DFA
  ( DfaInfo (..)
  , alphabetCardinality
  , fromERE
  , indexToChar
  )
import Regexle.PuzzleCache
  ( Puzzle (..)
  )
import Regexle.DfaCache (getCachedDfa)
import Regexle.RegexParser (parseRegexToERE)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import System.IO
  ( BufferMode (NoBuffering)
  , Handle
  , IOMode (WriteMode)
  , hClose
  , hFlush
  , hPutStrLn
  , hSetBuffering
  , openFile
  , stderr
  )
import Text.Printf (printf)
import qualified Z3.Base as Z3Base
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Types and configuration
-------------------------------------------------------------------------------

data Z3TransitionEncoding = Z3TransitionLegacy | Z3TransitionLambda
  deriving (Eq, Show)

data SolveBackend
  = BackendZ3Direct
  | BackendZ3PyClone
  deriving (Eq, Show)

data SolveConfig = SolveConfig
  { scBackend :: !SolveBackend
  , scZ3TransitionEncoding :: !Z3TransitionEncoding
  }
  deriving (Eq, Show)

defaultSolveConfig :: SolveConfig
defaultSolveConfig =
  SolveConfig
    { scBackend = BackendZ3PyClone
    , scZ3TransitionEncoding = Z3TransitionLambda
    }

enableZ3ConsistencyChecks :: IO ()
enableZ3ConsistencyChecks = do
  flag <- lookupEnv "REGEXLE_Z3_CHECKS"
  Z3Base.globalParamResetAll
  let enabled =
        case fmap (map toLower) flag of
          Just val | val `elem` ["0", "false", "off", "no"] -> False
          _ -> True
  when enabled $ do
    -- Turn on Z3 reference-count consistency checks so we get a trapped failure instead of a segfault.
    Z3Base.globalParamSet "debug_ref_count" "true"

pycloneDebug :: Bool
pycloneDebug = unsafePerformIO $ do
  flag <- lookupEnv "REGEXLE_PYCLONE_DEBUG"
  pure (maybe False (not . null) flag)
{-# NOINLINE pycloneDebug #-}

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
  , srStats :: !(Maybe T.Text)
  }
  deriving (Eq, Show)

toSeconds :: NominalDiffTime -> Double
toSeconds = realToFrac

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

solvePuzzle :: Puzzle -> IO (Either String SolveResult)
solvePuzzle = solvePuzzleWith defaultSolveConfig

solvePuzzleWith :: SolveConfig -> Puzzle -> IO (Either String SolveResult)
solvePuzzleWith cfg puzzle =
  case scBackend cfg of
    BackendZ3PyClone -> do
      eClues <- buildCluesCached puzzle
      case eClues of
        Left err -> pure (Left err)
        Right clues -> solvePuzzleWithClues cfg puzzle clues
    _ ->
      case buildClues puzzle of
        Left err -> pure (Left err)
        Right clues -> solvePuzzleWithClues cfg puzzle clues

solvePuzzleWithClues :: SolveConfig -> Puzzle -> [Clue] -> IO (Either String SolveResult)
solvePuzzleWithClues cfg puzzle clues =
  case scBackend cfg of
    BackendZ3Direct -> solvePuzzleZ3Direct (scZ3TransitionEncoding cfg) puzzle clues
    BackendZ3PyClone -> solvePuzzleZ3PyClone cfg puzzle clues

solvePuzzlesHot :: SolveConfig -> [(Puzzle, [Clue])] -> IO [Either String SolveResult]
solvePuzzlesHot cfg puzzlesWithClues =
  case scBackend cfg of
    BackendZ3Direct ->
      solvePuzzlesZ3DirectHot Nothing (scZ3TransitionEncoding cfg) puzzlesWithClues
    BackendZ3PyClone ->
      mapM (\(puzzle, clues) -> solvePuzzleZ3PyClone cfg puzzle clues) puzzlesWithClues

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

data TransitionFunction
  = TransitionLambda !Z3.AST
  | TransitionSubstitute
      { tfParamState :: !Z3.AST
      , tfParamChar :: !Z3.AST
      , tfExpr :: !Z3.AST
      }

data ConstraintSink = ConstraintSink
  { csEmit :: !(Z3.AST -> Z3.Z3 ())
  , csDeclare :: !(Z3.AST -> Z3.Z3 ())
  }

solverSink :: ConstraintSink
solverSink = ConstraintSink { csEmit = Z3.assert, csDeclare = const (pure ()) }

data HotBaseSignature = HotBaseSignature
  { hbsAlphabetSize :: !Int
  , hbsStateSize :: !Int
  , hbsGridDim :: !Int
  }
  deriving (Eq, Show)

data HotDumpSpec = HotDumpSpec
  { hdsDir :: !FilePath
  , hdsBaseFile :: !FilePath
  , hdsDriverFile :: !FilePath
  , hdsBaseDumped :: !(IORef (Maybe HotBaseSignature))
  , hdsPuzzleFiles :: !(IORef [(Int, FilePath)])
  }

data PuzzleLogTarget = PuzzleLogTarget
  { pltHandle :: !Handle
  , pltPath :: !FilePath
  , pltEvalBuffer :: !(IORef [String])
  }

hotDumpBaseFile :: HotDumpSpec -> FilePath
hotDumpBaseFile = hdsBaseFile

mkHotDumpSpec :: FilePath -> IO HotDumpSpec
mkHotDumpSpec dir = do
  createDirectoryIfMissing True dir
  baseRef <- newIORef Nothing
  filesRef <- newIORef []
  let basePath = dir </> "base.smt2"
      driverPath = dir </> "driver.smt2"
  writeFile driverPath . unlines $
    [ printf "; hot solver driver for %s" dir
    , printf "; replay via: cd %s && z3 driver.smt2" dir
    , "(set-logic ALL)"
    ]
  pure HotDumpSpec
    { hdsDir = dir
    , hdsBaseFile = basePath
    , hdsDriverFile = driverPath
    , hdsBaseDumped = baseRef
    , hdsPuzzleFiles = filesRef
    }

hotDumpPuzzleFile :: HotDumpSpec -> Int -> FilePath
hotDumpPuzzleFile spec day = hdsDir spec </> printf "puzzle-%04d.smt2" day

recordPuzzleDump :: HotDumpSpec -> Int -> FilePath -> IO ()
recordPuzzleDump spec day path =
  modifyIORef' (hdsPuzzleFiles spec) (<> [(day, path)])

maybeDumpBase :: Maybe HotDumpSpec -> AlphabetDomain -> StateDomain -> [[Z3.AST]] -> Z3.Z3 ()
maybeDumpBase Nothing _ _ _ = pure ()
maybeDumpBase (Just spec) alphabetDomain stateDomain grid = do
  let signature = mkHotBaseSignature alphabetDomain stateDomain grid
  recorded <- liftIO (readIORef (hdsBaseDumped spec))
  case recorded of
    Just existing
      | existing == signature -> pure ()
      | otherwise ->
          liftIO $ ioError (userError baseMismatchMsg)
    Nothing -> do
      baseText <- renderBaseDump alphabetDomain stateDomain grid
      liftIO $ do
        writeFile (hdsBaseFile spec) baseText
        writeIORef (hdsBaseDumped spec) (Just signature)
        appendDriver spec (hdsBaseFile spec)
  where
    baseMismatchMsg =
      unlines
        [ "Hot dump directory " ++ hdsDir spec ++ " already contains a base context"
        , "with incompatible dimensions or domain sizes."
        , "Choose an empty directory or delete the previous dump before retrying."
        ]

renderBaseDump :: AlphabetDomain -> StateDomain -> [[Z3.AST]] -> Z3.Z3 String
renderBaseDump alphabetDomain stateDomain grid = do
  alphabetDecl <- renderDatatypeDecl (adSort alphabetDomain) (adValues alphabetDomain)
  stateDecl <- renderDatatypeDecl (sdSort stateDomain) (sdValues stateDomain)
  cellDecls <- renderGridDecls (adSort alphabetDomain) grid
  let commentLine = printf "; dim=%d alphabet=%d state=%d" (length grid) (V.length (adValues alphabetDomain)) (V.length (sdValues stateDomain))
  pure . unlines $
    ["; shared hot solver base", commentLine]
      ++ [alphabetDecl, stateDecl]
      ++ cellDecls

renderDatatypeDecl :: Z3.Sort -> V.Vector Z3.AST -> Z3.Z3 String
renderDatatypeDecl sort values = do
  sortName <- Z3.sortToString sort
  ctorNames <- mapM Z3.astToString (V.toList values)
  let ctorList = unwords (map wrapCtor ctorNames)
  pure $ "(declare-datatypes ((" ++ sortName ++ " 0)) ((" ++ ctorList ++ ")))"
  where
    wrapCtor name = "(" ++ name ++ ")"

renderGridDecls :: Z3.Sort -> [[Z3.AST]] -> Z3.Z3 [String]
renderGridDecls sort grid = do
  sortName <- Z3.sortToString sort
  cellNames <- mapM (mapM Z3.astToString) grid
  pure ["(declare-const " ++ cell ++ " " ++ sortName ++ ")" | cell <- concat cellNames]

mkHotBaseSignature :: AlphabetDomain -> StateDomain -> [[Z3.AST]] -> HotBaseSignature
mkHotBaseSignature alphabetDomain stateDomain grid =
  HotBaseSignature
    { hbsAlphabetSize = V.length (adValues alphabetDomain)
    , hbsStateSize = V.length (sdValues stateDomain)
    , hbsGridDim = length grid
    }

buildLoggingSink :: Maybe HotDumpSpec -> Puzzle -> ConstraintSink -> Z3.Z3 (ConstraintSink, Maybe PuzzleLogTarget)
buildLoggingSink Nothing _ sink = pure (sink, Nothing)
buildLoggingSink (Just spec) puzzle sink = do
  let path = hotDumpPuzzleFile spec (puzzleDay puzzle)
  handle <- liftIO $ openFile path WriteMode
  liftIO $ hSetBuffering handle NoBuffering
  evalRef <- liftIO $ newIORef []
  liftIO $ do
    hPutStrLn handle $ printf "; puzzle %d" (puzzleDay puzzle)
    hPutStrLn handle "(push)"
  liftIO $ recordPuzzleDump spec (puzzleDay puzzle) path
  let target = PuzzleLogTarget
        { pltHandle = handle
        , pltPath = path
        , pltEvalBuffer = evalRef
        }
  let loggingSink = ConstraintSink
        { csEmit = \ast -> do
            csEmit sink ast
            rendered <- Z3.astToString ast
            liftIO $ hPutStrLn (pltHandle target) ("(assert " ++ rendered ++ ")")
          , csDeclare = \ast -> do
              declStr <- Z3.astToString ast
              sort <- Z3.getSort ast
              sortStr <- Z3.sortToString sort
              liftIO $ hPutStrLn (pltHandle target) ("(declare-const " ++ declStr ++ " " ++ sortStr ++ ")")
          }
  pure (loggingSink, Just target)

finalizePuzzleLog :: Maybe HotDumpSpec -> Maybe PuzzleLogTarget -> String -> IO ()
finalizePuzzleLog _ Nothing _ = pure ()
finalizePuzzleLog mSpec (Just target) comment = do
  let handle = pltHandle target
  hPutStrLn handle "(check-sat)"
  hPutStrLn handle "(get-model)"
  flushEvalBuffer target
  hPutStrLn handle $ "; result: " ++ comment
  hPutStrLn handle "(pop)"
  hFlush handle
  hClose handle
  forM_ mSpec $ \spec -> appendDriver spec (pltPath target)

flushEvalBuffer :: PuzzleLogTarget -> IO ()
flushEvalBuffer target = do
  cmds <- readIORef (pltEvalBuffer target)
  mapM_ (hPutStrLn (pltHandle target)) (reverse cmds)

finalizeHotDump :: HotDumpSpec -> IO ()
finalizeHotDump _ = pure ()

appendDriver :: HotDumpSpec -> FilePath -> IO ()
appendDriver spec path =
  appendFile (hdsDriverFile spec) $ "(include \"" ++ takeFileName path ++ "\")\n"

mkAlphabetDomain :: Int -> Z3.Z3 AlphabetDomain
mkAlphabetDomain n = do
  let size = max 1 n
  sort <- Z3.mkIntSort
  values <- V.generateM size $ \idx -> Z3.mkIntNum (toInteger idx)
  pure AlphabetDomain { adSort = sort, adValues = values }

mkStateDomain :: String -> Int -> Z3.Z3 StateDomain
mkStateDomain _label n = do
  let size = max 1 n
  sort <- Z3.mkIntSort
  values <- V.generateM size $ \idx -> Z3.mkIntNum (toInteger idx)
  pure StateDomain { sdSort = sort, sdValues = values }

alphabetLiteral :: AlphabetDomain -> Int -> Z3.AST
alphabetLiteral dom idx = adValues dom V.! idx

stateLiteral :: StateDomain -> Int -> Z3.AST
stateLiteral dom idx = sdValues dom V.! idx

alphabetDomainSize :: AlphabetDomain -> Int
alphabetDomainSize = V.length . adValues

stateDomainSize :: StateDomain -> Int
stateDomainSize = V.length . sdValues

assertAlphabetBounds :: (Z3.AST -> Z3.Z3 ()) -> AlphabetDomain -> Z3.AST -> Z3.Z3 ()
assertAlphabetBounds emit domain cell = do
  zero <- Z3.mkIntNum (0 :: Integer)
  geZero <- Z3.mkGe cell zero
  emit geZero
  limit <- Z3.mkIntNum (toInteger (alphabetDomainSize domain))
  ltLimit <- Z3.mkLt cell limit
  emit ltLimit

assertStateBounds :: (Z3.AST -> Z3.Z3 ()) -> Int -> Z3.AST -> Z3.Z3 ()
assertStateBounds emit stateLimit st = do
  zero <- Z3.mkIntNum (0 :: Integer)
  geZero <- Z3.mkGe st zero
  emit geZero
  when (stateLimit > 0) $ do
    limit <- Z3.mkIntNum (toInteger stateLimit)
    ltLimit <- Z3.mkLt st limit
    emit ltLimit

solvePuzzleZ3Direct :: Z3TransitionEncoding -> Puzzle -> [Clue] -> IO (Either String SolveResult)
solvePuzzleZ3Direct encoding puzzle clues = do
  buildStart <- getCurrentTime
  stageRef <- newIORef "initializing"
  enableZ3ConsistencyChecks
  let stateCounts = map (max 1 . V.length . diTransitions . clueDfa) clues
      globalStateCount = maximum (1 : stateCounts)
  resultOrErr <- (try $ Z3.evalZ3 $ do
    alphabetDomain <- mkAlphabetDomain alphabetCardinality
    stateDomain <- mkStateDomain "GlobalState" globalStateCount
    liftIO (writeIORef stageRef "mkGrid")
    grid <- mkGridZ3 alphabetDomain (puzzleDiameter puzzle)
    forM_ grid $ \row ->
      forM_ row $ \cell ->
        assertAlphabetBounds (csEmit solverSink) alphabetDomain cell
    forM_ (zip clues stateCounts) $ \(clue, stateLimit) -> do
      liftIO (writeIORef stageRef ("clue " ++ clueLabel clue))
      applyClueZ3 solverSink encoding alphabetDomain stateDomain stateLimit grid clue
    liftIO (writeIORef stageRef "solverCheck")
    solveStart <- liftIO getCurrentTime
    (res, mModel) <- Z3.solverCheckAndGetModel
    solveEnd <- liftIO getCurrentTime
    statsText <- Z3.solverGetStatistics
    case (res, mModel) of
      (Z3.Sat, Just model) -> do
        liftIO (writeIORef stageRef "extractModel")
        values <- extractGridValues Nothing alphabetDomain puzzle grid model
        pure (Just values, solveStart, solveEnd, statsText)
      _ -> pure (Nothing, solveStart, solveEnd, statsText)
    ) :: IO (Either SomeException (Maybe [[Word8]], UTCTime, UTCTime, String))
  case resultOrErr of
    Left ex -> do
      stage <- readIORef stageRef
      pure (Left ("Z3 solver failed during " ++ stage ++ ": " ++ displayException ex))
    Right (Nothing, _, _, statsStr) -> pure (Left ("Z3 solver reported UNSAT/UNKNOWN (stats: " ++ statsStr ++ ")"))
    Right (Just modelVals, solveStart, solveEnd, statsStr) -> do
      let buildTime = toSeconds (diffUTCTime solveStart buildStart)
          solveTime = toSeconds (diffUTCTime solveEnd solveStart)
          rendered = renderGrid puzzle modelVals
      pure (Right SolveResult {srGrid = rendered, srBuildTime = buildTime, srSolveTime = solveTime, srStats = Just (T.pack statsStr)})

data StateBanKey = StateBanKey !Char !Int !Int
  deriving (Eq, Ord)

type StateBanCache = Map.Map StateBanKey IntSet.IntSet

solvePuzzleZ3PyClone :: SolveConfig -> Puzzle -> [Clue] -> IO (Either String SolveResult)
solvePuzzleZ3PyClone _cfg puzzle clues = do
  buildStart <- getCurrentTime
  stageRef <- newIORef "initializing"
  enableZ3ConsistencyChecks
  let stateCounts = map (max 1 . V.length . diTransitions . clueDfa) clues
      globalStateCount = maximum (1 : stateCounts)
  resultOrErr <- (try $ Z3.evalZ3 $ do
    alphabetDomain <- mkAlphabetDomain alphabetCardinality
    stateDomain <- mkStateDomain "PyClone" globalStateCount
    goal <- Z3.mkGoal True False False
    liftIO (writeIORef stageRef "mkGrid")
    let dim = puzzleDiameter puzzle
    grid <- mkGridZ3 alphabetDomain dim
    alphabetBanRef <- liftIO (newIORef IntMap.empty)
    stateBanRef <- liftIO (newIORef Map.empty)
    let emitBounds = Z3.goalAssert goal
    forM_ grid $ \row ->
      forM_ row $ \cell ->
        assertAlphabetBounds emitBounds alphabetDomain cell
    forM_ clues $ \clue -> do
      liftIO (writeIORef stageRef ("clue " ++ clueLabel clue))
      applyClueZ3PyClone goal dim alphabetBanRef stateBanRef alphabetDomain stateDomain grid clue
    formulas <- Z3.getGoalFormulas goal
    mapM_ Z3.solverAssertCnstr formulas
    liftIO (writeIORef stageRef "solverCheck")
    mDump <- liftIO (lookupEnv "REGEXLE_PYCLONE_DUMP_DIR")
    forM_ mDump $ \dir -> do
      solverText <- Z3.solverToString
      let fileName = printf "pyclone-%03d.smt2" (puzzleDay puzzle)
          outPath = dir </> fileName
      liftIO $ createDirectoryIfMissing True dir
      liftIO $ writeFile outPath solverText
    solveStart <- liftIO getCurrentTime
    (res, mModel) <- Z3.solverCheckAndGetModel
    solveEnd <- liftIO getCurrentTime
    statsText <- Z3.solverGetStatistics
    case (res, mModel) of
      (Z3.Sat, Just model) -> do
        liftIO (writeIORef stageRef "extractModel")
        values <- extractGridValues Nothing alphabetDomain puzzle grid model
        pure (Just values, solveStart, solveEnd, statsText)
      _ -> pure (Nothing, solveStart, solveEnd, statsText)
    ) :: IO (Either SomeException (Maybe [[Word8]], UTCTime, UTCTime, String))
  case resultOrErr of
    Left ex -> do
      stage <- readIORef stageRef
      pure (Left ("Z3 solver failed during " ++ stage ++ ": " ++ displayException ex))
    Right (Nothing, _, _, statsStr) ->
      pure (Left ("Z3 solver reported UNSAT/UNKNOWN (stats: " ++ statsStr ++ ")"))
    Right (Just modelVals, solveStart, solveEnd, statsStr) -> do
      let buildTime = toSeconds (diffUTCTime solveStart buildStart)
          solveTime = toSeconds (diffUTCTime solveEnd solveStart)
          rendered = renderGrid puzzle modelVals
      pure (Right SolveResult { srGrid = rendered
                              , srBuildTime = buildTime
                              , srSolveTime = solveTime
                              , srStats = Just (T.pack statsStr)
                              })

-------------------------------------------------------------------------------
-- Hot direct Z3 backend
-------------------------------------------------------------------------------

solvePuzzlesZ3DirectHotWithDump :: Maybe FilePath -> Int -> Bool -> Z3TransitionEncoding -> [(Puzzle, [Clue])] -> IO [Either String SolveResult]
solvePuzzlesZ3DirectHotWithDump dumpDir chunkLimit dryRun encoding jobs = do
  enableZ3ConsistencyChecks
  mSpec <- traverse mkHotDumpSpec dumpDir
  let runChunks = fmap concat $ mapM (solveChunk mSpec) (chunkList chunkLimit jobs)
  case mSpec of
    Nothing -> runChunks
    Just spec -> runChunks `finally` finalizeHotDump spec
  where
    solveChunk _ [] = pure []
    solveChunk spec chunk@((firstPuzzle, _) : _)
      | not (sameDimensions chunk && sameSides chunk) = mapM (uncurry (solvePuzzleZ3Direct encoding)) chunk
      | otherwise = do
          stageRef <- newIORef "initializing hot Z3"
          resultOrErr <- (try $ Z3.evalZ3 $ do
            alphabetDomain <- mkAlphabetDomain alphabetCardinality
            stateDomain <- mkStateDomain "GlobalStateHot" (chunkStateCount chunk)
            grid <- mkGridZ3 alphabetDomain (puzzleDiameter firstPuzzle)
            forM_ grid $ \row ->
              forM_ row $ \cell ->
                assertAlphabetBounds (csEmit solverSink) alphabetDomain cell
            maybeDumpBase spec alphabetDomain stateDomain grid
            forM chunk $ \(puzzle, clues) -> do
              liftIO (writeIORef stageRef ("puzzle " ++ show (puzzleDay puzzle)))
              (sink, mHandle) <- buildLoggingSink spec puzzle solverSink
              result <-
                if dryRun
                  then Z3.local $ do
                    forM_ clues $ \clue -> do
                      let stateLimit = max 1 (V.length (diTransitions (clueDfa clue)))
                      applyClueZ3 sink encoding alphabetDomain stateDomain stateLimit grid clue
                    pure (Left "dry-run (constraints dumped only)")
                  else Z3.local $ do
                    puzzleBuildStart <- liftIO getCurrentTime
                    forM_ clues $ \clue -> do
                      let stateLimit = max 1 (V.length (diTransitions (clueDfa clue)))
                      applyClueZ3 sink encoding alphabetDomain stateDomain stateLimit grid clue
                    solveStart <- liftIO getCurrentTime
                    (res, mModel) <- Z3.solverCheckAndGetModel
                    solveEnd <- liftIO getCurrentTime
                    statsText <- Z3.solverGetStatistics
                    case (res, mModel) of
                      (Z3.Sat, Just model) -> do
                        values <- extractGridValues (fmap pltEvalBuffer mHandle) alphabetDomain puzzle grid model
                        let buildTime = toSeconds (diffUTCTime solveStart puzzleBuildStart)
                            solveTime = toSeconds (diffUTCTime solveEnd solveStart)
                            rendered = renderGrid puzzle values
                        pure $ Right
                          SolveResult
                            { srGrid = rendered
                            , srBuildTime = buildTime
                            , srSolveTime = solveTime
                            , srStats = Just (T.pack statsText)
                            }
                      (Z3.Unsat, _) -> pure (Left "Z3 solver reported UNSAT")
                      _ -> pure (Left "Z3 solver reported UNKNOWN")
              liftIO $ finalizePuzzleLog spec mHandle (either id (const "sat") result)
              pure result
            ) :: IO (Either SomeException [Either String SolveResult])
          case resultOrErr of
            Left ex -> do
              stage <- readIORef stageRef
              pure (replicate (length chunk) (Left ("Z3 hot solver failed during " ++ stage ++ ": " ++ displayException ex)))
            Right results' -> pure results'

sameDimensions :: [(Puzzle, [Clue])] -> Bool
sameDimensions chunk =
  case chunk of
    [] -> True
    ((p0, _) : rest) -> all ((== puzzleDiameter p0) . puzzleDiameter . fst) rest

sameSides :: [(Puzzle, [Clue])] -> Bool
sameSides chunk =
  case chunk of
    [] -> True
    ((p0, _) : rest) -> all ((== puzzleSide p0) . puzzleSide . fst) rest

chunkStateCount :: [(Puzzle, [Clue])] -> Int
chunkStateCount chunk =
  let stateCounts = map (max 1 . V.length . diTransitions . clueDfa) (concatMap snd chunk)
   in maximum (1 : stateCounts)

solvePuzzlesZ3DirectHot :: Maybe FilePath -> Z3TransitionEncoding -> [(Puzzle, [Clue])] -> IO [Either String SolveResult]
solvePuzzlesZ3DirectHot dumpDir encoding jobs =
  solvePuzzlesZ3DirectHotWithDump dumpDir maxHotBatchSize False encoding jobs

solvePuzzlesZ3DirectHotWithLimit :: Z3TransitionEncoding -> Int -> [(Puzzle, [Clue])] -> IO [Either String SolveResult]
solvePuzzlesZ3DirectHotWithLimit encoding chunkLimit jobs =
  solvePuzzlesZ3DirectHotWithDump Nothing chunkLimit False encoding jobs

solvePuzzlesZ3DirectHotNoChunk :: Maybe FilePath -> Z3TransitionEncoding -> [(Puzzle, [Clue])] -> IO [Either String SolveResult]
solvePuzzlesZ3DirectHotNoChunk dumpDir encoding jobs =
  solvePuzzlesZ3DirectHotWithDump dumpDir 0 False encoding jobs

maxHotBatchSize :: Int
maxHotBatchSize = 5

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs
  | n <= 0 = [xs]
  | otherwise = go xs
  where
    go [] = []
    go ys =
      let (prefix, rest) = splitAt n ys
       in prefix : go rest

mkGridZ3 :: AlphabetDomain -> Int -> Z3.Z3 [[Z3.AST]]
mkGridZ3 alphabetDomain dim =
  forM [0 .. dim - 1] $ \x ->
    forM [0 .. dim - 1] $ \y ->
      Z3.mkFreshConst ("cell_" ++ show x ++ "_" ++ show y) (adSort alphabetDomain)

applyClueZ3PyClone :: Z3Base.Goal -> Int -> IORef (IntMap.IntMap IntSet.IntSet) -> IORef StateBanCache -> AlphabetDomain -> StateDomain -> [[Z3.AST]] -> Clue -> Z3.Z3 ()
applyClueZ3PyClone goal dim alphabetBansRef stateBansRef alphabetDomain stateDomain grid clue = do
  let chars = map (\(x, y) -> grid !! x !! y) (clueCoords clue)
      indexedChars = zip (clueCoords clue) chars
      info = clueDfa clue
      deadStates = diDeadStates info
      deadAlphabet = diDeadAlphabet info
      initialDead =
        let vec = diDeadFrom info
         in if V.null vec then IntSet.empty else vec V.! diInitial info
      clueLen = length chars
      stateCount = V.length (diTransitions info)
      deadAlphabetCount = IntSet.size deadAlphabet
      deadInitCount = IntSet.size initialDead
      deadStateCount = IntSet.size deadStates
      alphabetCap = alphabetDomainSize alphabetDomain
      stateDomainCap = stateDomainSize stateDomain
      stateVarCount = clueLen + 1
      deadAlphabetIdxs =
        [ idx
        | idx <- IntSet.toList deadAlphabet
        , idx >= 0
        , idx < alphabetCap
        ]
      initialDeadIdxs =
        [ idx
        | idx <- IntSet.toList initialDead
        , idx >= 0
        , idx < alphabetCap
        ]
      alphabetBanCounts =
        [ if idx == 0
            then IntSet.size (deadAlphabet `IntSet.union` initialDead)
            else deadAlphabetCount
        | idx <- [0 .. clueLen - 1]
        ]
      stateBanContribution = stateVarCount * length deadStateIdxs
      diseqEstimate = sum alphabetBanCounts + stateBanContribution
      deadStateIdxs =
        [ idx
        | idx <- IntSet.toList deadStates
        , idx >= 0
        , idx < stateCount
        ]
      bannedStateIdxs = IntSet.toList (IntSet.fromList deadStateIdxs)
  when pycloneDebug $ liftIO $ do
    hPutStrLn stderr $ printf
      "pyclone clue %c%d len=%d states=%d deadAlpha=%d deadInit=%d deadStates=%d alphaDomain=%d stateDomain=%d estBannedNeq=%d"
      (clueAxis clue)
      (clueIndex clue)
      clueLen
      stateCount
      deadAlphabetCount
      deadInitCount
      deadStateCount
      alphabetCap
      stateDomainCap
      diseqEstimate
  transitionFn <- buildTransitionFunction Z3TransitionLambda stateDomain alphabetDomain info
  states <- mkStateVarsZ3 solverSink stateDomain (length chars) (clueAxis clue) (clueIndex clue)
  let goalSink = ConstraintSink { csEmit = Z3.goalAssert goal, csDeclare = const (pure ()) }
      emit ast = Z3.goalAssert goal ast
      stateKey offset = StateBanKey (clueAxis clue) (clueIndex clue) offset
  forM_ (zip [0 ..] states) $ \(stateOffset, st) -> do
    assertStateBounds emit stateCount st
    newStateBans <- liftIO $ recordStateBans (stateKey stateOffset) bannedStateIdxs
    forM_ newStateBans $ \idx -> do
      let lit = stateLiteral stateDomain idx
      mkDistinctNeq st lit >>= emit
  let firstCoord = case clueCoords clue of
        [] -> Nothing
        (c:_) -> Just c
  forM_ indexedChars $ \(coord, cell) -> do
    let key = cellIndex coord
    newGeneral <- liftIO $ recordBans key deadAlphabetIdxs
    forM_ newGeneral $ \idx -> do
      let lit = alphabetLiteral alphabetDomain idx
      mkDistinctNeq cell lit >>= emit
    case firstCoord of
      Just fc | coord == fc -> do
        newInitial <- liftIO $ recordBans key initialDeadIdxs
        forM_ newInitial $ \idx -> do
          let lit = alphabetLiteral alphabetDomain idx
          mkDistinctNeq cell lit >>= emit
      _ -> pure ()
  case states of
    (st0 : _) -> do
      let initLit = stateLiteral stateDomain (diInitial info)
      eq <- Z3.mkEq st0 initLit
      emit eq
    _ -> pure ()
  applyTransitionsZ3 goalSink transitionFn states chars
  case lastMaybe states of
    Just finalState -> assertAcceptStateZ3 goalSink stateDomain info finalState
    Nothing -> pure ()
  where
    mkDistinctNeq a b = Z3.mkDistinct [a, b]
    cellIndex (x, y) = x * dim + y
    recordBans key idxs =
      atomicModifyIORef' alphabetBansRef $ \m ->
        let current = IntMap.findWithDefault IntSet.empty key m
            newIndices = filter (`IntSet.notMember` current) idxs
            updated = IntSet.union current (IntSet.fromList newIndices)
            newMap = IntMap.insert key updated m
         in (newMap, newIndices)
    recordStateBans key idxs =
      atomicModifyIORef' stateBansRef $ \m ->
        let current = Map.findWithDefault IntSet.empty key m
            newIndices = filter (`IntSet.notMember` current) idxs
            updated = IntSet.union current (IntSet.fromList newIndices)
            newMap = Map.insert key updated m
         in (newMap, newIndices)

applyClueZ3 :: ConstraintSink -> Z3TransitionEncoding -> AlphabetDomain -> StateDomain -> Int -> [[Z3.AST]] -> Clue -> Z3.Z3 ()
applyClueZ3 sink encoding alphabetDomain stateDomain stateLimit grid clue = do
  let chars = map (\(x, y) -> grid !! x !! y) (clueCoords clue)
      dfaInfo = clueDfa clue
      deadStates = diDeadStates dfaInfo
      deadAlphabet = diDeadAlphabet dfaInfo
      initialDead =
        let vec = diDeadFrom dfaInfo
         in if V.null vec then IntSet.empty else vec V.! diInitial dfaInfo
  transitionFn <- buildTransitionFunction encoding stateDomain alphabetDomain dfaInfo
  states <- mkStateVarsZ3 sink stateDomain (length chars) (clueAxis clue) (clueIndex clue)
  restrictStatesZ3 sink stateDomain stateLimit deadStates states
  mapM_ (restrictAlphabetZ3 sink alphabetDomain deadAlphabet) chars
  case (states, chars) of
    (st0 : _, firstChar : _) -> do
      restrictAlphabetZ3 sink alphabetDomain initialDead firstChar
      let initLit = stateLiteral stateDomain (diInitial dfaInfo)
      eq <- Z3.mkEq st0 initLit
      csEmit sink eq
    (st0 : _, []) -> do
      let initLit = stateLiteral stateDomain (diInitial dfaInfo)
      eq <- Z3.mkEq st0 initLit
      csEmit sink eq
    _ -> pure ()
  applyTransitionsZ3 sink transitionFn states chars
  case lastMaybe states of
    Just finalState -> assertAcceptStateZ3 sink stateDomain dfaInfo finalState
    Nothing -> pure ()

mkStateVarsZ3 :: ConstraintSink -> StateDomain -> Int -> Char -> Int -> Z3.Z3 [Z3.AST]
mkStateVarsZ3 sink stateDomain len axis idx =
  forM [0 .. len] $ \i ->
    do
      ast <- Z3.mkFreshConst ("state_" ++ [axis] ++ "_" ++ show idx ++ "_" ++ show i) (sdSort stateDomain)
      csDeclare sink ast
      pure ast

restrictStatesZ3 :: ConstraintSink -> StateDomain -> Int -> IntSet.IntSet -> [Z3.AST] -> Z3.Z3 ()
restrictStatesZ3 sink stateDomain stateLimit deadStates states = do
  let emit = csEmit sink
      domainSize = stateDomainSize stateDomain
      limit = max 0 (min stateLimit domainSize)
      deadIdxs =
        [ idx
        | idx <- IntSet.toList deadStates
        , idx >= 0
        , idx < limit
        ]
  forM_ states $ \st -> do
    assertStateBounds emit limit st
    forM_ deadIdxs $ \idx -> do
      let lit = stateLiteral stateDomain idx
      eq <- Z3.mkEq st lit
      neq <- Z3.mkNot eq
      emit neq

restrictAlphabetZ3 :: ConstraintSink -> AlphabetDomain -> IntSet.IntSet -> Z3.AST -> Z3.Z3 ()
restrictAlphabetZ3 sink alphabetDomain banned cell =
  forM_ (IntSet.toList banned) $ \idx -> do
    let idxLit = alphabetLiteral alphabetDomain idx
    eq <- Z3.mkEq cell idxLit
    neq <- Z3.mkNot eq
    csEmit sink neq

applyTransitionsZ3 :: ConstraintSink -> TransitionFunction -> [Z3.AST] -> [Z3.AST] -> Z3.Z3 ()
applyTransitionsZ3 _ _ _ [] = pure ()
applyTransitionsZ3 _ _ [_] _ = pure ()
applyTransitionsZ3 sink transFn (prev:next:restStates) (ch:chs) = do
  expected <- applyTransitionFunction transFn prev ch
  eq <- Z3.mkEq next expected
  csEmit sink eq
  applyTransitionsZ3 sink transFn (next:restStates) chs
applyTransitionsZ3 _ _ _ _ = pure ()

assertAcceptStateZ3 :: ConstraintSink -> StateDomain -> DfaInfo -> Z3.AST -> Z3.Z3 ()
assertAcceptStateZ3 sink stateDomain info finalState =
  case IntSet.toList (diAccepting info) of
    [] -> do
      falseNode <- Z3.mkFalse
      csEmit sink falseNode
    xs -> do
      comparisons <- forM xs $ \v -> do
        let lit = stateLiteral stateDomain v
        Z3.mkEq finalState lit
      cond <- Z3.mkOr comparisons
      csEmit sink cond

buildTransitionLambdaBody :: StateDomain -> AlphabetDomain -> DfaInfo -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST
buildTransitionLambdaBody stateDomain alphabetDomain info st ch = do
  statePairs <- forM (zip [0 :: Int ..] (V.toList (diTransitions info))) $ \(idx, row) -> do
    rowExpr <- buildRowExpr row
    cond <- Z3.mkEq st (stateLiteral stateDomain idx)
    pure (cond, rowExpr)
  case statePairs of
    [] -> pure (stateLiteral stateDomain 0)
    _ -> do
      let (_, fallbackExpr) = last statePairs
      buildIteLadder statePairs fallbackExpr
  where
    buildRowExpr row = do
      let fallbackIdx = if V.null row then 0 else V.last row
          fallbackLit = stateLiteral stateDomain fallbackIdx
      classPairs <- fmap catMaybes $
        forM (V.toList (diColumnClasses info)) (buildClassPair row fallbackIdx)
      buildIteLadder classPairs fallbackLit

    buildClassPair row fallbackIdx members = do
      let idxs = V.toList members
      case idxs of
        [] -> pure Nothing
        repIdx : rest -> do
          mCond <- buildClassCondition (repIdx : rest)
          case mCond of
            Nothing -> pure Nothing
            Just cond -> do
              let destIdx =
                    if repIdx < V.length row
                      then row V.! repIdx
                      else fallbackIdx
              pure (Just (cond, stateLiteral stateDomain destIdx))

    buildClassCondition [] = pure Nothing
    buildClassCondition (firstIdx : restIdxs) = do
      eqs <- forM (firstIdx : restIdxs) $ \idx -> do
        let lit = alphabetLiteral alphabetDomain idx
        Z3.mkEq ch lit
      case eqs of
        [] -> pure Nothing
        [single] -> pure (Just single)
        _ -> Just <$> Z3.mkOr eqs

buildIteLadder :: [(Z3.AST, Z3.AST)] -> Z3.AST -> Z3.Z3 Z3.AST
buildIteLadder pairs fallback = do
  let step (cond, val) acc = Z3.mkIte cond val acc
  foldrM step fallback pairs

transitionLookupZ3 :: StateDomain -> AlphabetDomain -> DfaInfo -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST
transitionLookupZ3 stateDomain alphabetDomain info st ch = do
  let transitions = diTransitions info
      classes = diColumnClasses info
  classTests <- V.mapM (buildClassTest ch) classes
  rowExprs <- V.mapM (buildRowExpr classTests) transitions
  buildStateExpr (zip [0 :: Int ..] (V.toList rowExprs))
  where
    buildClassTest cell members = do
      let idxs = V.toList members
      eqs <- forM idxs $ \idx -> do
        let lit = alphabetLiteral alphabetDomain idx
        Z3.mkEq cell lit
      case eqs of
        [] -> Z3.mkFalse
        [single] -> pure single
        _ -> Z3.mkOr eqs

    buildRowExpr classTests row = do
      let classesVec = diColumnClasses info
          classCount = V.length classTests
          fallback = if V.null row then 0 else V.last row
      dests <- V.generateM classCount $ \classIdx -> do
        let members = classesVec V.! classIdx
            repIdx = if V.null members then 0 else members V.! 0
            dstIdx =
              if repIdx < V.length row
                then row V.! repIdx
                else fallback
        pure (stateLiteral stateDomain dstIdx)
      let pairs = zip (V.toList classTests) (V.toList dests)
      case pairs of
        [] -> pure (stateLiteral stateDomain fallback)
        _ -> buildClassExpr pairs

    buildClassExpr pairs = do
      let (initPairs, lastPair) = case pairs of
            [] -> ([], Nothing)
            _  -> (init pairs, Just (last pairs))
      base <- case lastPair of
        Nothing -> pure (stateLiteral stateDomain 0)
        Just (_, dst) -> pure dst
      foldrM step base initPairs
      where
        step (cond, dst) acc = Z3.mkIte cond dst acc

    buildStateExpr [] = pure (stateLiteral stateDomain 0)
    buildStateExpr pairs = do
      let (initPairs, lastPair) = (init pairs, last pairs)
          (_, lastRow) = lastPair
      foldrM step lastRow initPairs
      where
        step (idx, rowExpr) acc = do
          let idxLit = stateLiteral stateDomain idx
          cond <- Z3.mkEq st idxLit
          Z3.mkIte cond rowExpr acc

foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ acc [] = pure acc
foldrM f acc (x:xs) = do
  rest <- foldrM f acc xs
  f x rest

buildTransitionFunction :: Z3TransitionEncoding -> StateDomain -> AlphabetDomain -> DfaInfo -> Z3.Z3 TransitionFunction
buildTransitionFunction encoding stateDomain alphabetDomain info =
  case encoding of
    Z3TransitionLegacy -> buildSubstitute
    Z3TransitionLambda -> buildLambda
  where
    buildSubstitute = do
      stateParam <- Z3.mkFreshConst "delta_state" (sdSort stateDomain)
      charParam <- Z3.mkFreshConst "delta_char" (adSort alphabetDomain)
      expr <- transitionLookupZ3 stateDomain alphabetDomain info stateParam charParam
      pure TransitionSubstitute
        { tfParamState = stateParam
        , tfParamChar = charParam
        , tfExpr = expr
        }
    buildLambda = do
      let stateSort = sdSort stateDomain
          charSort = adSort alphabetDomain
      stateBound <- Z3.mkBound 1 stateSort
      charBound <- Z3.mkBound 0 charSort
      body <- buildTransitionLambdaBody stateDomain alphabetDomain info stateBound charBound
      stateSym <- Z3.mkStringSymbol "lambda_state"
      charSym <- Z3.mkStringSymbol "lambda_char"
      outer <- Z3.mkLambda [(stateSym, stateSort), (charSym, charSort)] body
      pure (TransitionLambda outer)

applyTransitionFunction :: TransitionFunction -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST
applyTransitionFunction transFn st ch =
  case transFn of
    TransitionLambda func -> Z3.mkSelectN func [st, ch]
    TransitionSubstitute { tfParamState = stateParam
                         , tfParamChar = charParam
                         , tfExpr = expr
                         } ->
      Z3.substitute expr [(stateParam, st), (charParam, ch)]

clueLabel :: Clue -> String
clueLabel clue =
  let patternPreview = take 24 (T.unpack (cluePattern clue))
      prefix = [clueAxis clue] ++ show (clueIndex clue)
   in prefix ++ if null patternPreview then "" else " " ++ patternPreview

extractCellValue :: Maybe (IORef [String]) -> AlphabetDomain -> Z3.Model -> Z3.AST -> Z3.Z3 Word8
extractCellValue mEvalLog alphabetDomain model cell = do
  forM_ mEvalLog $ \ref -> do
    cellStr <- Z3.astToString cell
    liftIO $ modifyIORef' ref ( ("(get-value (" ++ cellStr ++ "))") : )
  mVal <- Z3.modelEval model cell True
  case mVal of
    Nothing -> liftIO (ioError (userError "Z3 model missing cell value"))
    Just ast -> do
      intVal <- Z3.getInt ast
      let idx = fromIntegral intVal :: Int
      when (idx < 0 || idx >= alphabetDomainSize alphabetDomain) $ do
        rendered <- Z3.astToString ast
        liftIO (ioError (userError ("Alphabet index out of range: " ++ rendered)))
      pure (fromIntegral idx :: Word8)

extractGridValues :: Maybe (IORef [String]) -> AlphabetDomain -> Puzzle -> [[Z3.AST]] -> Z3.Model -> Z3.Z3 [[Word8]]
extractGridValues mEvalLog alphabetDomain puzzle grid model =
  forM (zip [0 :: Int ..] grid) $ \(xIdx, row) ->
    forM (zip [0 :: Int ..] row) $ \(yIdx, cell) ->
      if abs (xIdx - yIdx) >= puzzleSide puzzle
        then pure 0
        else extractCellValue mEvalLog alphabetDomain model cell

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

buildCluesCached :: Puzzle -> IO (Either String [Clue])
buildCluesCached puzzle = do
  let side = puzzleSide puzzle
      dim = puzzleDiameter puzzle
      collect axisFn axisChar patterns =
        map (\(idx, pat) -> (axisChar, idx, pat, axisFn idx)) (zip [0 ..] patterns)
      requests =
        collect (axisX dim side) 'x' (puzzleX puzzle)
          ++ collect (axisY dim side) 'y' (puzzleY puzzle)
          ++ collect (axisZ dim side) 'z' (puzzleZ puzzle)
  results <- forM requests buildCached
  pure (sequence results)
  where
    buildCached (axis, idx, pat, coords) = do
      eDfa <- getCachedDfa pat
      pure $ case eDfa of
        Left err -> Left (formatError axis idx pat err)
        Right dfa ->
          Right
            Clue
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
