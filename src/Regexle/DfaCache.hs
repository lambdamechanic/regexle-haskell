{-# LANGUAGE OverloadedStrings #-}

module Regexle.DfaCache
  ( getCachedDfa
  ) where

import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , eitherDecodeFileStrict'
  , encode
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Char (ord)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Text (Text)
import Numeric (showHex)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , renameFile
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import Regexle.DFA
  ( DfaInfo (..)
  , fromERE
  )
import Regexle.PuzzleCache (cacheRoot)
import Regexle.RegexParser (parseRegexToERE)

cacheVersion :: Int
cacheVersion = 1

{-# NOINLINE cacheRef #-}
cacheRef :: IORef (Map Text DfaInfo)
cacheRef = unsafePerformIO (newIORef Map.empty)

data DfaPayload = DfaPayload
  { payloadTransitions :: ![[Int]]
  , payloadAccepting :: ![Int]
  , payloadInitial :: !Int
  , payloadDeadStates :: ![Int]
  , payloadDeadAlphabet :: ![Int]
  , payloadDeadFrom :: ![[Int]]
  }

instance ToJSON DfaPayload where
  toJSON DfaPayload { payloadTransitions = transitions
                    , payloadAccepting = accepting
                    , payloadInitial = initial
                    , payloadDeadStates = deadStates
                    , payloadDeadAlphabet = deadAlphabet
                    , payloadDeadFrom = deadFrom
                    } =
    object
      [ "transitions" .= transitions
      , "accepting" .= accepting
      , "initial" .= initial
      , "dead_states" .= deadStates
      , "dead_alphabet" .= deadAlphabet
      , "dead_from" .= deadFrom
      ]

instance FromJSON DfaPayload where
  parseJSON = withObject "DfaPayload" $ \o ->
    DfaPayload
      <$> o .: "transitions"
      <*> o .: "accepting"
      <*> o .: "initial"
      <*> o .: "dead_states"
      <*> o .: "dead_alphabet"
      <*> o .: "dead_from"

payloadFromDfa :: DfaInfo -> DfaPayload
payloadFromDfa DfaInfo { diTransitions = trans
                       , diAccepting = accepting
                       , diInitial = initial
                       , diDeadStates = deadStates
                       , diDeadAlphabet = deadAlphabet
                       , diDeadFrom = deadFrom
                       } =
  DfaPayload
    { payloadTransitions = map V.toList (V.toList trans)
    , payloadAccepting = IntSet.toList accepting
    , payloadInitial = initial
    , payloadDeadStates = IntSet.toList deadStates
    , payloadDeadAlphabet = IntSet.toList deadAlphabet
    , payloadDeadFrom = map IntSet.toList (V.toList deadFrom)
    }

dfaFromPayload :: DfaPayload -> DfaInfo
dfaFromPayload DfaPayload { payloadTransitions = trans
                          , payloadAccepting = accepting
                          , payloadInitial = initial
                          , payloadDeadStates = deadStates
                          , payloadDeadAlphabet = deadAlphabet
                          , payloadDeadFrom = deadFrom
                          } =
  let rows = V.fromList (map V.fromList trans)
      (columnClasses, columnClassOf) = buildColumnClasses rows
   in DfaInfo
        { diTransitions = rows
        , diAccepting = IntSet.fromList accepting
        , diInitial = initial
        , diDeadStates = IntSet.fromList deadStates
        , diDeadAlphabet = IntSet.fromList deadAlphabet
        , diDeadFrom = V.fromList (map IntSet.fromList deadFrom)
        , diColumnClasses = columnClasses
        , diColumnClassOf = columnClassOf
        }

buildColumnClasses :: V.Vector (V.Vector Int) -> (V.Vector (V.Vector Int), V.Vector Int)
buildColumnClasses rows
  | charCount <= 0 = (V.empty, V.empty)
  | otherwise =
      let signatures = [ (idx, columnSignature idx) | idx <- [0 .. charCount - 1] ]
          classMap =
            foldl'
              ( \acc (idx, sig) ->
                  Map.insertWith (++) sig [idx] acc
              )
              Map.empty
              signatures
          classLists = map reverse (Map.elems classMap)
          classVec = V.fromList (map V.fromList classLists)
          assignments =
            concat
              [ [ (member, classIdx)
                | member <- members
                ]
              | (classIdx, members) <- zip [0 :: Int ..] classLists
              ]
          classOf =
            if charCount <= 0
              then V.empty
              else V.accum
                     (\_ new -> new)
                     (V.replicate charCount 0)
                     assignments
       in (classVec, classOf)
  where
    charCount
      | V.null rows = 0
      | otherwise = V.length (V.head rows)
    columnSignature idx =
      V.toList (V.map (\row -> row V.! idx) rows)

getCachedDfa :: Text -> IO (Either String DfaInfo)
getCachedDfa patternText = do
  memo <- readIORef cacheRef
  case Map.lookup patternText memo of
    Just dfa -> pure (Right dfa)
    Nothing -> do
      cachePath <- patternCachePath patternText
      cached <- doesFileExist cachePath
      if cached
        then do
          ePayload <- eitherDecodeFileStrict' cachePath
          case ePayload of
            Right payload -> do
              let dfa = dfaFromPayload payload
              insertCache patternText dfa
              pure (Right dfa)
            Left err -> do
              hPutStrLn stderr $
                "Warning: Failed to decode DFA cache at "
                  ++ cachePath
                  ++ ": "
                  ++ err
              buildAndStore cachePath
        else buildAndStore cachePath
  where
    buildAndStore cachePath = do
      case parseRegexToERE patternText of
        Left err -> pure (Left err)
        Right ere -> do
          let dfa = fromERE ere
              payload = payloadFromDfa dfa
          writeCache cachePath payload
          insertCache patternText dfa
          pure (Right dfa)

insertCache :: Text -> DfaInfo -> IO ()
insertCache patternText dfa =
  modifyIORef' cacheRef (Map.insert patternText dfa)

writeCache :: FilePath -> DfaPayload -> IO ()
writeCache path payload = do
  createDirectoryIfMissing True (takeDirectory path)
  let tmpPath = path ++ ".tmp"
  BL.writeFile tmpPath (encode payload)
  renameFile tmpPath path

patternCachePath :: Text -> IO FilePath
patternCachePath patternText = do
  root <- cacheRoot
  pure $
    root
      </> "dfa"
      </> ("v" ++ show cacheVersion)
      </> (toHex patternText ++ ".json")

toHex :: Text -> String
toHex =
  concatMap (pad4 . (`showHex` ""))
    . map ord
    . T.unpack

pad4 :: String -> String
pad4 str =
  let len = length str
      needed = (4 - (len `mod` 4)) `mod` 4
   in replicate needed '0' ++ str
