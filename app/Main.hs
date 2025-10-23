{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Data.Aeson (Value (Null), encode, object, (.=), toJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Options.Applicative
import Regexle.PuzzleCache
  ( Puzzle (..)
  , fetchPuzzle
  )
import Regexle.Solver
  ( SolveResult (..)
  , solvePuzzle
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

data Command
  = CmdFetch FetchOptions
  | CmdMatrix MatrixOptions
  | CmdProfile ProfileOptions
  deriving (Show)

data FetchOptions = FetchOptions
  { foDay :: !Int
  , foSide :: !Int
  }
  deriving (Show)

data MatrixOptions = MatrixOptions
  { moSideStart :: !Int
  , moSideEnd :: !Int
  , moDayStart :: !Int
  , moDayEnd :: !Int
  , moOutput :: !FilePath
  }
  deriving (Show)

data ProfileOptions = ProfileOptions
  { poSide :: !Int
  , poDays :: ![Int]
  , poOutput :: !FilePath
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
      auto
      ( long "side-start"
          <> metavar "INT"
          <> help "Minimum side (inclusive)"
          <> showDefault
          <> value 3
      )
    <*> option
      auto
      ( long "side-end"
          <> metavar "INT"
          <> help "Maximum side (inclusive)"
          <> showDefault
          <> value 8
      )
    <*> option
      auto
      ( long "day-start"
          <> metavar "INT"
          <> help "Minimum day (inclusive)"
          <> showDefault
          <> value 400
      )
    <*> option
      auto
      ( long "day-end"
          <> metavar "INT"
          <> help "Maximum day (inclusive)"
          <> showDefault
          <> value 449
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
      (eitherReader parseDaySpec)
      ( long "days"
          <> metavar "SPEC"
          <> help "Day list (e.g. 400..409 or 400,401,405)"
          <> showDefault
          <> value [400 .. 409]
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output path for profile JSON"
          <> showDefault
          <> value "stats/profile.json"
      )

runFetch :: FetchOptions -> IO ()
runFetch FetchOptions {foDay = day, foSide = side} = do
  puzzle <- fetchPuzzle day side
  BL8.putStrLn (encode puzzle)

runMatrix :: MatrixOptions -> IO ()
runMatrix MatrixOptions { moSideStart = sideStart
                        , moSideEnd = sideEnd
                        , moDayStart = dayStart
                        , moDayEnd = dayEnd
                        , moOutput = outputPath
                        } = do
  let sides = [sideStart .. sideEnd]
      days = [dayStart .. dayEnd]
  entries <- fmap concat $
    forM sides $ \side -> do
      fmap concat $
        forM days $ \day -> do
          puzzle <- fetchPuzzle day side
          forM (strategySpecs side) $ \spec -> matrixEntry puzzle spec

  let outDir = takeDirectory outputPath
  createDirectoryIfMissing True outDir
  BL.writeFile outputPath (encode entries)
  putStrLn $
    "Wrote "
      <> show (length entries)
      <> " rows to "
      <> outputPath

data StrategySpec = StrategySpec
  { ssName :: !String
  , ssConfig :: !Value
  }

strategySpecs :: Int -> [StrategySpec]
strategySpecs _ = [StrategySpec "sbv_dfa" (object [])]

matrixEntry :: Puzzle -> StrategySpec -> IO Value
matrixEntry puzzle spec = do
  result <- solvePuzzle puzzle
  let baseFields =
        [ "side" .= puzzleSide puzzle
        , "day" .= puzzleDay puzzle
        , "strategy" .= ssName spec
        , "strategy_config" .= ssConfig spec
        ]
  case result of
    Left err ->
      pure $ object (baseFields ++ ["error" .= err, "build_time" .= Null, "solve_time" .= Null, "z3_stats" .= object []])
    Right SolveResult { srBuildTime = buildTime, srSolveTime = solveTime, srGrid = gridLines } ->
      pure $
        object
          ( baseFields
              ++ [ "build_time" .= buildTime
                 , "solve_time" .= solveTime
                 , "solution" .= map T.pack gridLines
                 , "z3_stats" .= object []
                 ]
          )

runProfile :: ProfileOptions -> IO ()
runProfile ProfileOptions {poSide = side, poDays = days, poOutput = outputPath} = do
  runs <- forM days $ \day -> do
    puzzle <- fetchPuzzle day side
    result <- solvePuzzle puzzle
    let base =
          [ "side" .= side
          , "day" .= day
          ]
    case result of
      Left err ->
        pure (object (base ++ ["error" .= err]), Nothing, Nothing)
      Right SolveResult { srBuildTime = buildTime, srSolveTime = solveTime, srGrid = gridLines } ->
        let entry =
              object
                ( base
                    ++ [ "build_time" .= buildTime
                       , "solve_time" .= solveTime
                       , "solution" .= map T.pack gridLines
                       , "z3_stats" .= object []
                       ]
                )
         in pure (entry, Just buildTime, Just solveTime)

  let totalRuns = length runs
      successes = [ (b, s) | (_, Just b, Just s) <- runs ]
      solvedCount = length successes
      avgBuild = average (map fst successes)
      avgSolve = average (map snd successes)
      entries = map (\(entry, _, _) -> entry) runs
      summaryValue =
        object
          [ "requested" .= totalRuns
          , "solved" .= solvedCount
          , "avg_build_time" .= maybe Null toJSON avgBuild
          , "avg_solve_time" .= maybe Null toJSON avgSolve
          ]

  let outDir = takeDirectory outputPath
  createDirectoryIfMissing True outDir
  BL.writeFile outputPath (encode (object ["runs" .= entries, "summary" .= summaryValue]))

  putStrLn $
    "Profiled "
      <> show totalRuns
      <> " puzzles; solved "
      <> show solvedCount
      <> ". Avg build "
      <> displayMaybe avgBuild
      <> "s, avg solve "
      <> displayMaybe avgSolve
      <> "s. Results written to "
      <> outputPath

  where
    average :: [Double] -> Maybe Double
    average [] = Nothing
    average xs = Just (sum xs / fromIntegral (length xs))

    displayMaybe :: Maybe Double -> String
    displayMaybe = maybe "n/a" show

parseDaySpec :: String -> Either String [Int]
parseDaySpec spec =
  fmap concat . traverse parseChunk $ map trim (splitComma spec)
  where
    splitComma :: String -> [String]
    splitComma = foldr step [""]
      where
        step ',' acc = "" : acc
        step c (x : xs) = (c : x) : xs
        step _ [] = []

    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace

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
