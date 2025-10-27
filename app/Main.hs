{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_, when)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON, Value (Null), encode, object, (.=), toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Options.Applicative
import Regexle.PuzzleCache
  ( Puzzle (..)
  , fetchPuzzle
  )
import Regexle.Solver
  ( SolveBackend (..)
  , SolveConfig (..)
  , SolveResult (..)
  , TransitionEncoding (..)
  , Z3TransitionEncoding (..)
  , defaultSolveConfig
  , buildClues
  , solvePuzzleWith
  , solvePuzzleWithClues
  , solvePuzzlesHot
  , solvePuzzlesZ3DirectHotWithDump
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    CmdFetch fetchOpts -> runFetch fetchOpts
    CmdMatrix matrixOpts -> runMatrix matrixOpts
    CmdProfile profileOpts -> runProfile profileOpts
    CmdReproHot reproOpts -> runReproHot reproOpts

data Command
  = CmdFetch FetchOptions
  | CmdMatrix MatrixOptions
  | CmdProfile ProfileOptions
  | CmdReproHot ReproOptions
  deriving (Show)

data FetchOptions = FetchOptions
  { foDay :: !Int
  , foSide :: !Int
  }
  deriving (Show)

data SolverStrategy
  = StrategySBV TransitionEncoding
  | StrategyDirectZ3 !Z3TransitionEncoding
  deriving (Eq, Show)

data MatrixOptions = MatrixOptions
  { moSides :: ![Int]
  , moDays :: ![Int]
  , moStrategies :: ![SolverStrategy]
  , moOutput :: !FilePath
  }
  deriving (Show)

data ProfileOptions = ProfileOptions
  { poSide :: !Int
  , poDays :: ![Int]
  , poHotSolver :: !Bool
  , poStrategy :: !SolverStrategy
  , poOutput :: !FilePath
  }
  deriving (Show)

data ReproOptions = ReproOptions
  { roSide :: !Int
  , roDays :: ![Int]
  , roChunkSize :: !Int
  , roDumpDir :: !(Maybe FilePath)
  }
  deriving (Show)

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Regexle utilities: fetch puzzles and generate benchmarking matrices."
    )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "fetch"
        ( info
            (CmdFetch <$> fetchOptionsParser)
            (progDesc "Fetch and print a puzzle JSON for a given day and side")
        )
        <> command
          "matrix"
          ( info
              (CmdMatrix <$> matrixOptionsParser)
              (progDesc "Generate benchmark matrix JSON for strategy combinations")
          )
        <> command
          "profile"
          ( info
              (CmdProfile <$> profileOptionsParser)
              (progDesc "Aggregate benchmark stats for a range of puzzle days")
          )
        <> command
          "repro-hot"
          ( info
              (CmdReproHot <$> reproOptionsParser)
              (progDesc "Directly exercise the hot Z3 solver on a custom day range (debug helper)")
          )
    )

fetchOptionsParser :: Parser FetchOptions
fetchOptionsParser =
  FetchOptions
    <$> argument auto (metavar "DAY")
    <*> argument auto (metavar "SIDE")

matrixOptionsParser :: Parser MatrixOptions
matrixOptionsParser =
  MatrixOptions
    <$> option
      (eitherReader parseRangeSpec)
      ( long "sides"
          <> metavar "SPEC"
          <> help "Side list (e.g. 3..5 or 3,4)"
          <> showDefault
          <> value [3 .. 8]
      )
    <*> option
      (eitherReader parseRangeSpec)
      ( long "days"
          <> metavar "SPEC"
          <> help "Day list (e.g. 400..409 or 400,401)"
          <> showDefault
          <> value [400 .. 449]
      )
    <*> option
      (eitherReader parseStrategyList)
      ( long "strategies"
          <> metavar "LIST"
          <> help "Comma list of solver strategies (lookup,lambda,enum,z3,z3-legacy)"
          <> value [StrategySBV UseLookup, StrategySBV UseLambda, StrategyDirectZ3 Z3TransitionLambda]
          <> showDefaultWith (const "lookup,lambda,z3")
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output path for matrix JSON"
          <> showDefault
          <> value "stats/matrix.json"
      )

profileOptionsParser :: Parser ProfileOptions
profileOptionsParser =
  ProfileOptions
    <$> option
      auto
      ( long "side"
          <> metavar "INT"
          <> help "Puzzle side length"
          <> showDefault
          <> value 3
      )
    <*> option
      (eitherReader parseRangeSpec)
      ( long "days"
          <> metavar "SPEC"
          <> help "Day list (e.g. 400..409 or 400,401,405)"
          <> showDefault
          <> value [400 .. 409]
      )
    <*> switch
      ( long "hot-solver"
          <> help "Reuse a single solver session across all puzzles (experimental)"
      )
    <*> option
      (eitherReader parseStrategySingle)
      ( long "strategy"
          <> metavar "NAME"
          <> help "Solver strategy (lookup, lambda, enum, z3, or z3-legacy)"
          <> showDefaultWith (const "lambda")
          <> value (StrategySBV UseLambda)
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output path for profile JSON"
          <> showDefault
          <> value "stats/profile.json"
      )

reproOptionsParser :: Parser ReproOptions
reproOptionsParser =
  ReproOptions
    <$> option
      auto
      ( long "side"
          <> metavar "INT"
          <> help "Puzzle side length"
          <> showDefault
          <> value 3
      )
    <*> option
      (eitherReader parseRangeSpec)
      ( long "days"
          <> metavar "SPEC"
          <> help "Day list (e.g. 30..35)"
          <> showDefaultWith (const "30..35")
          <> value [30 .. 35]
      )
    <*> option
      auto
      ( long "chunk-size"
          <> metavar "INT"
          <> help "Maximum puzzles per hot batch (<=0 disables chunking)"
          <> showDefault
          <> value 0
      )
    <*> optional
      ( strOption
          ( long "dump-dir"
              <> metavar "DIR"
              <> help "Optional directory to dump SMT2 files and driver"
          )
      )

runFetch :: FetchOptions -> IO ()
runFetch FetchOptions {foDay = day, foSide = side} = do
  puzzle <- fetchPuzzle day side
  BL8.putStrLn (encode puzzle)

runMatrix :: MatrixOptions -> IO ()
runMatrix MatrixOptions { moSides = sides
                        , moDays = days
                        , moStrategies = strategies
                        , moOutput = outputPath
                        } = do
  entries <- fmap concat $
    forM sides $ \side -> do
      fmap concat $
        forM days $ \day -> do
          puzzle <- fetchPuzzle day side
          forM (strategySpecs strategies) $ \spec -> matrixEntry puzzle spec

  let outDir = takeDirectory outputPath
  createDirectoryIfMissing True outDir
  BL.writeFile outputPath (encode entries)
  putStrLn $
    "Wrote "
      <> show (length entries)
      <> " rows to "
      <> outputPath

runReproHot :: ReproOptions -> IO ()
runReproHot ReproOptions { roSide = side
                         , roDays = days
                         , roChunkSize = chunkSize
                         , roDumpDir = dumpDir
                         } = do
  when (null days) $ fail "repro-hot: no days provided"
  putStrLn $ "Fetching " <> show (length days) <> " puzzles"
  puzzlesWithClues <- forM days $ \day -> do
    puzzle <- fetchPuzzle day side
    case buildClues puzzle of
      Left err -> fail ("Failed to build clues for day " <> show day <> ": " <> err)
      Right clues -> pure (puzzle, clues)

  let solveCfg = defaultSolveConfig { scBackend = BackendZ3Direct }
      encoding = scZ3TransitionEncoding solveCfg
      runAction = solvePuzzlesZ3DirectHotWithDump dumpDir chunkSize encoding puzzlesWithClues

  putStrLn $ "Invoking hot solver with chunk size = " <> show chunkSize
  results <- runAction
  forM_ (zip days results) $ \(day, outcome) ->
    case outcome of
      Left err ->
        putStrLn $ "Day " <> show day <> ": ERROR - " <> err
      Right SolveResult { srBuildTime = buildT, srSolveTime = solveT } ->
        putStrLn $
          "Day "
            <> show day
            <> ": build="
            <> show buildT
            <> "s solve="
            <> show solveT
            <> "s"

data StrategySpec = StrategySpec
  { ssName :: !String
  , ssConfig :: !Value
  , ssSolveConfig :: !SolveConfig
  }

strategySpecs :: [SolverStrategy] -> [StrategySpec]
strategySpecs = map strategySpecFrom

strategySpecFrom :: SolverStrategy -> StrategySpec
strategySpecFrom strat =
  case strat of
    StrategySBV encoding ->
      let label = strategyLabel strat
          solverCfg =
            defaultSolveConfig
              { scBackend = BackendSBV
              , scTransitionEncoding = encoding
              }
       in StrategySpec
            { ssName = T.unpack label
            , ssConfig = object
                [ "backend" .= ("sbv" :: T.Text)
                , "transition" .= strategyKernelLabel encoding
                ]
            , ssSolveConfig = solverCfg
            }
    StrategyDirectZ3 z3Enc ->
      StrategySpec
        { ssName = T.unpack (strategyLabel (StrategyDirectZ3 z3Enc))
        , ssConfig = object
            [ "backend" .= ("z3_direct" :: T.Text)
            , "transition" .= z3TransitionKernelLabel z3Enc
            ]
        , ssSolveConfig = defaultSolveConfig
            { scBackend = BackendZ3Direct
            , scZ3TransitionEncoding = z3Enc
            }
        }

matrixEntry :: Puzzle -> StrategySpec -> IO Value
matrixEntry puzzle spec = do
  result <- solvePuzzleWith (ssSolveConfig spec) puzzle
  let baseFields =
        [ "side" .= puzzleSide puzzle
        , "day" .= puzzleDay puzzle
        , "strategy" .= ssName spec
        , "strategy_config" .= ssConfig spec
        ]
  case result of
    Left err ->
      pure $
        object
          ( baseFields
              ++ [ "error" .= err
                 , "build_time" .= Null
                 , "solve_time" .= Null
                 , "total_time" .= Null
                 , "z3_stats" .= object []
                 ]
          )
    Right SolveResult { srBuildTime = buildTime
                      , srSolveTime = solveTime
                      , srGrid = gridLines
                      , srStats = statsTxt
                      } ->
      pure $
        object
          ( baseFields
              ++ [ "build_time" .= buildTime
                 , "solve_time" .= solveTime
                 , "total_time" .= (buildTime + solveTime)
                 , "solution" .= map T.pack gridLines
                 , "z3_stats" .= statsValue statsTxt
                 ]
          )
  where
    statsValue Nothing = object []
    statsValue (Just txt) = object ["raw" .= txt]

runProfile :: ProfileOptions -> IO ()
runProfile ProfileOptions { poSide = side
                          , poDays = days
                          , poHotSolver = useHot
                          , poStrategy = strategy
                          , poOutput = outputPath
                          } = do
  let strategySpec = strategySpecFrom strategy
      strategyNameText = T.pack (ssName strategySpec)
      strategyConfigValue = ssConfig strategySpec
      solverCfg = ssSolveConfig strategySpec

  puzzleEntries <- forM days $ \day -> do
    puzzle <- fetchPuzzle day side
    case buildClues puzzle of
      Left err -> pure (day, Left err)
      Right clues -> pure (day, Right (puzzle, clues))

  let jobs = [ (day, puzzle, clues) | (day, Right (puzzle, clues)) <- puzzleEntries ]

  overallStart <- getCurrentTime

  jobResults <-
    if null jobs
      then pure []
      else
        if useHot
          then solvePuzzlesHot solverCfg (map (\(_, puzzle, clues) -> (puzzle, clues)) jobs)
          else mapM (\(_, puzzle, clues) -> solvePuzzleWithClues solverCfg puzzle clues) jobs

  overallEnd <- getCurrentTime

  let jobAssoc = Map.fromList (zip (map (\(day, _, _) -> day) jobs) jobResults)
      rows =
        [ case entry of
            Left err ->
              ProfileRow
                { prSide = side
                , prDay = day
                , prBuildTime = Nothing
                , prSolveTime = Nothing
                , prTotalTime = Nothing
                , prSolution = Nothing
                , prError = Just (T.pack err)
                , prStrategy = strategyNameText
                , prStrategyConfig = strategyConfigValue
                , prZ3Stats = object []
                }
            Right _ ->
              case Map.lookup day jobAssoc of
                Nothing ->
                  ProfileRow
                    { prSide = side
                    , prDay = day
                    , prBuildTime = Nothing
                    , prSolveTime = Nothing
                    , prTotalTime = Nothing
                    , prSolution = Nothing
                    , prError = Just "Internal error: missing hot solver result"
                    , prStrategy = strategyNameText
                    , prStrategyConfig = strategyConfigValue
                    , prZ3Stats = object []
                    }
                Just (Left err) ->
                  ProfileRow
                    { prSide = side
                    , prDay = day
                    , prBuildTime = Nothing
                    , prSolveTime = Nothing
                    , prTotalTime = Nothing
                    , prSolution = Nothing
                    , prError = Just (T.pack err)
                    , prStrategy = strategyNameText
                    , prStrategyConfig = strategyConfigValue
                    , prZ3Stats = object []
                    }
                Just (Right SolveResult { srBuildTime = buildTime
                                         , srSolveTime = solveTime
                                         , srGrid = gridLines
                                         , srStats = statsTxt
                                         }) ->
                  ProfileRow
                    { prSide = side
                    , prDay = day
                    , prBuildTime = Just buildTime
                    , prSolveTime = Just solveTime
                    , prTotalTime = Just (buildTime + solveTime)
                    , prSolution = Just (map T.pack gridLines)
                    , prError = Nothing
                    , prStrategy = strategyNameText
                    , prStrategyConfig = strategyConfigValue
                    , prZ3Stats = statsValue statsTxt
                    }
        | (day, entry) <- puzzleEntries
        ]

  let totalRuns = length rows
      buildSamples = catMaybes (map prBuildTime rows)
      solveSamples = catMaybes (map prSolveTime rows)
      solvedCount = length buildSamples
      avgBuild = average buildSamples
      avgSolve = average solveSamples
      wallSeconds = toSeconds (diffUTCTime overallEnd overallStart)
      jsonValue = profileToJson rows wallSeconds

  let outDir = takeDirectory outputPath
  createDirectoryIfMissing True outDir
  BL.writeFile outputPath (encode jsonValue)

  putStrLn $
    "Profiled "
      <> show totalRuns
      <> " puzzles; solved "
      <> show solvedCount
      <> ". Avg build "
      <> displayMaybe avgBuild
      <> "s, avg solve "
      <> displayMaybe avgSolve
      <> "s, total wall "
      <> show wallSeconds
      <> "s. Results written to "
      <> outputPath

  where
    average :: [Double] -> Maybe Double
    average [] = Nothing
    average xs = Just (sum xs / fromIntegral (length xs))

    toSeconds :: NominalDiffTime -> Double
    toSeconds = realToFrac

    displayMaybe :: Maybe Double -> String
    displayMaybe = maybe "n/a" show

    statsValue :: Maybe T.Text -> Value
    statsValue Nothing = object []
    statsValue (Just txt) = object ["raw" .= txt]

data ProfileRow = ProfileRow
  { prSide :: !Int
  , prDay :: !Int
  , prBuildTime :: !(Maybe Double)
  , prSolveTime :: !(Maybe Double)
  , prTotalTime :: !(Maybe Double)
  , prSolution :: !(Maybe [T.Text])
  , prError :: !(Maybe T.Text)
  , prStrategy :: !T.Text
  , prStrategyConfig :: !Value
  , prZ3Stats :: !Value
  }

profileToJson :: [ProfileRow] -> Double -> Value
profileToJson rows wallSeconds =
  object
    ( columnPayload
        ++ [ "_meta"
             .= object
                [ "wall_time_seconds" .= wallSeconds
                , "rows" .= length rows
                ]
           ]
    )
  where
    columnPayload =
      [ "side" .= columnFrom (map (toJSON . prSide) rows)
      , "day" .= columnFrom (map (toJSON . prDay) rows)
      , "strategy" .= columnFrom (map (toJSON . prStrategy) rows)
      , "strategy_config" .= columnFrom (map prStrategyConfig rows)
      , "build_time" .= columnFrom (map (valueOrNull . prBuildTime) rows)
      , "solve_time" .= columnFrom (map (valueOrNull . prSolveTime) rows)
      , "total_time" .= columnFrom (map (valueOrNull . prTotalTime) rows)
      , "solution" .= columnFrom (map (valueOrNull . prSolution) rows)
      , "z3_stats" .= columnFrom (map prZ3Stats rows)
      , "error" .= columnFrom (map (valueOrNull . prError) rows)
      ]

valueOrNull :: ToJSON a => Maybe a -> Value
valueOrNull = maybe Null toJSON

columnFrom :: [Value] -> Value
columnFrom vals =
  object $
    zipWith
      (\idx val -> Key.fromText (T.pack (show idx)) .= val)
      [0 :: Int ..]
      vals

strategyKernelLabel :: TransitionEncoding -> T.Text
strategyKernelLabel UseLookup = "lookup"
strategyKernelLabel UseLambda = "lambda"
strategyKernelLabel UseEnum = "enum"

z3TransitionKernelLabel :: Z3TransitionEncoding -> T.Text
z3TransitionKernelLabel Z3TransitionLambda = "lambda"
z3TransitionKernelLabel Z3TransitionLegacy = "legacy"

strategyLabel :: SolverStrategy -> T.Text
strategyLabel (StrategySBV enc) = "sbv_" <> strategyKernelLabel enc
strategyLabel (StrategyDirectZ3 enc) = "z3_" <> z3TransitionKernelLabel enc

parseStrategySingle :: String -> Either String SolverStrategy
parseStrategySingle raw =
  case map toLower (trimSpaces raw) of
    "lookup" -> Right (StrategySBV UseLookup)
    "lambda" -> Right (StrategySBV UseLambda)
    "enum" -> Right (StrategySBV UseEnum)
    "z3" -> Right (StrategyDirectZ3 Z3TransitionLambda)
    "direct" -> Right (StrategyDirectZ3 Z3TransitionLambda)
    "z3-direct" -> Right (StrategyDirectZ3 Z3TransitionLambda)
    "z3-lambda" -> Right (StrategyDirectZ3 Z3TransitionLambda)
    "z3-legacy" -> Right (StrategyDirectZ3 Z3TransitionLegacy)
    "z3-subst" -> Right (StrategyDirectZ3 Z3TransitionLegacy)
    other -> Left $ "Unknown strategy: " <> other <> " (expected lookup, lambda, enum, z3, or z3-legacy)"

parseStrategyList :: String -> Either String [SolverStrategy]
parseStrategyList spec = traverse parseStrategySingle (splitCommaFields spec)

parseRangeSpec :: String -> Either String [Int]
parseRangeSpec spec =
  fmap concat . traverse parseChunk $ splitCommaFields spec
  where
    parseChunk chunk =
      case break (== '.') chunk of
        (start, '.' : '.' : rest) ->
          case (reads start, reads rest) of
            ([(a, "")], [(b, "")]) -> Right [a .. b]
            _ -> Left $ "Invalid day range: " <> chunk
        _ ->
          case reads chunk of
            [(n, "")] -> Right [n]
            _ -> Left $ "Invalid day: " <> chunk

splitCommaFields :: String -> [String]
splitCommaFields = map trimSpaces . foldr step [""]
  where
    step ',' acc = "" : acc
    step c (x : xs) = (c : x) : xs
    step _ [] = []

trimSpaces :: String -> String
trimSpaces = dropWhileEnd isSpace . dropWhile isSpace
