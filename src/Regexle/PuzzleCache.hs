{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Regexle.PuzzleCache
  ( Puzzle (..)
  , cacheRoot
  , puzzleCachePath
  , fetchPuzzle
  ) where

import Control.Monad (unless)
import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toEncoding, toJSON)
  , eitherDecode
  , eitherDecodeFileStrict'
  , object
  , pairs
  , withObject
  , (.:)
  , (.=)
  )
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Network.HTTP.Client
  ( Manager
  , Request
  , Response
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
  ( XdgDirectory (XdgCache)
  , createDirectoryIfMissing
  , doesFileExist
  , getXdgDirectory
  )
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)

data Puzzle = Puzzle
  { puzzleDiameter :: !Int
  , puzzleName :: !Text
  , puzzleDay :: !Int
  , puzzleSide :: !Int
  , puzzleSeed :: !Int
  , puzzleCommitHash :: !Text
  , puzzleX :: ![Text]
  , puzzleY :: ![Text]
  , puzzleZ :: ![Text]
  }
  deriving (Eq, Show)

instance FromJSON Puzzle where
  parseJSON = withObject "Puzzle" $ \o ->
    Puzzle
      <$> o .: "diameter"
      <*> o .: "name"
      <*> o .: "day"
      <*> o .: "side"
      <*> o .: "seed"
      <*> o .: "commit_hash"
      <*> o .: "x"
      <*> o .: "y"
      <*> o .: "z"

instance ToJSON Puzzle where
  toJSON Puzzle {..} =
    let pairsList =
          [ "diameter" .= puzzleDiameter
          , "name" .= puzzleName
          , "day" .= puzzleDay
          , "side" .= puzzleSide
          , "seed" .= puzzleSeed
          , "commit_hash" .= puzzleCommitHash
          , "x" .= puzzleX
          , "y" .= puzzleY
          , "z" .= puzzleZ
          ]
     in object pairsList

  toEncoding Puzzle {..} =
    pairs $
      "diameter" .= puzzleDiameter
        <> "name" .= puzzleName
        <> "day" .= puzzleDay
        <> "side" .= puzzleSide
        <> "seed" .= puzzleSeed
        <> "commit_hash" .= puzzleCommitHash
        <> "x" .= puzzleX
        <> "y" .= puzzleY
        <> "z" .= puzzleZ

cacheRoot :: IO FilePath
cacheRoot = getXdgDirectory XdgCache "regexle"

puzzleCachePath :: Int -> Int -> IO FilePath
puzzleCachePath day side = do
  root <- cacheRoot
  pure $ root </> ("puzzle_" ++ show day ++ "_" ++ show side ++ ".json")

fetchPuzzle :: Int -> Int -> IO Puzzle
fetchPuzzle day side = do
  cachePath <- puzzleCachePath day side
  cached <- doesFileExist cachePath
  if cached
    then do
      ePuzzle <- eitherDecodeFileStrict' cachePath
      case ePuzzle of
        Right puzzle -> pure puzzle
        Left err -> do
          hPutStrLn stderr $
            "Warning: Failed to decode cached puzzle at "
              ++ cachePath
              ++ ": "
              ++ err
          downloadAndCache cachePath
    else downloadAndCache cachePath
  where
    downloadAndCache cachePath = do
      manager <- newManager tlsManagerSettings
      response <- requestPuzzle manager day side
      let status = statusCode $ responseStatus response
      unless (status == 200) $
        fail $
          "HTTP request failed with status "
            ++ show status
            ++ " while fetching puzzle"

      let body = responseBody response
      puzzle <-
        either (fail . ("Failed to decode puzzle JSON: " ++)) pure $
          eitherDecode body
      createDirectoryIfMissing True (takeDirectory cachePath)
      BL.writeFile cachePath body
      pure puzzle

requestPuzzle :: Manager -> Int -> Int -> IO (Response BL.ByteString)
requestPuzzle manager day side = do
  request <- buildRequest day side
  httpLbs request manager

buildRequest :: Int -> Int -> IO Request
buildRequest day side =
  parseRequest $
    "https://generator.regexle.com/api?day="
      ++ show day
      ++ "&side="
      ++ show side
