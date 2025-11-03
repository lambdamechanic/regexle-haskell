module Regexle.DFA
  ( DfaInfo (..)
  , alphabet
  , alphabetCardinality
  , indexToChar
  , charToIndex
  , fromDfa
  , fromERE
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.List (foldl')
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Function.Step.Discrete.Closed as SF
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Kleene.DFA as KDFA
import qualified Kleene.ERE as ERE

data DfaInfo = DfaInfo
  { diTransitions :: !(Vector (Vector Int))
  , diAccepting :: !IntSet
  , diInitial :: !Int
  , diDeadStates :: !IntSet
  , diDeadAlphabet :: !IntSet
  , diDeadFrom :: !(Vector IntSet)
  , diColumnClasses :: !(Vector (Vector Int))
  , diColumnClassOf :: !(Vector Int)
  }
  deriving (Eq, Show)

alphabet :: Vector Char
alphabet = V.fromList ['A' .. 'Z']

alphabetCardinality :: Int
alphabetCardinality = V.length alphabet

indexToChar :: Int -> Maybe Char
indexToChar idx
  | idx < 0 || idx >= alphabetCardinality = Nothing
  | otherwise = Just (alphabet V.! idx)

charToIndex :: Char -> Maybe Int
charToIndex ch = V.findIndex (== ch) alphabet

fromERE :: ERE.ERE Char -> DfaInfo
fromERE = fromDfa . KDFA.fromERE

fromDfa :: KDFA.DFA Char -> DfaInfo
fromDfa dfa =
  let trans = KDFA.dfaTransition dfa
      stateIds =
        IntSet.unions
          [ IntMap.keysSet trans
          , KDFA.dfaAcceptable dfa
          , KDFA.dfaBlackholes dfa
          , IntSet.singleton (KDFA.dfaInitial dfa)
          ]
      maxState = if IntSet.null stateIds then 0 else IntSet.findMax stateIds
      rows = V.generate (maxState + 1) (buildRow trans)
      columnData = buildColumnClasses rows
      columnClasses = fst columnData
      columnClassOf = snd columnData
      deadStates = KDFA.dfaBlackholes dfa
      deadAlphabet' = computeDeadAlphabet rows deadStates
      deadFromVec = computeDeadFrom rows deadStates
   in DfaInfo
        { diTransitions = rows
        , diAccepting = KDFA.dfaAcceptable dfa
        , diInitial = KDFA.dfaInitial dfa
        , diDeadStates = deadStates
        , diDeadAlphabet = deadAlphabet'
        , diDeadFrom = deadFromVec
        , diColumnClasses = columnClasses
        , diColumnClassOf = columnClassOf
        }
  where
    buildRow :: IntMap (SF.SF Char Int) -> Int -> Vector Int
    buildRow transitionMap state =
      case IntMap.lookup state transitionMap of
        Nothing -> V.replicate alphabetCardinality state
        Just sf -> V.map (\c -> sf SF.! c) alphabet

computeDeadAlphabet :: Vector (Vector Int) -> IntSet -> IntSet
computeDeadAlphabet rows deadStates
  | V.null rows = IntSet.empty
  | otherwise = go 0 IntSet.empty
  where
    charCount = V.length (V.head rows)
    go idx acc
      | idx >= charCount = acc
      | otherwise =
          let column = V.map (\row -> row V.! idx) rows
              allDest = IntSet.fromList (V.toList column)
           in case IntSet.toList allDest of
                [d] | IntSet.member d deadStates -> go (idx + 1) (IntSet.insert idx acc)
                _ -> go (idx + 1) acc

computeDeadFrom :: Vector (Vector Int) -> IntSet -> Vector IntSet
computeDeadFrom rows deadStates =
  V.map
    ( V.ifoldl'
        ( \acc idx dst ->
            if IntSet.member dst deadStates
              then IntSet.insert idx acc
              else acc
        )
        IntSet.empty
    )
    rows

buildColumnClasses :: Vector (Vector Int) -> (Vector (Vector Int), Vector Int)
buildColumnClasses rows
  | charCount <= 0 = (V.empty, V.empty)
  | otherwise =
      let signatures =
            [ (idx, columnSignature idx)
            | idx <- [0 .. charCount - 1]
            ]
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
