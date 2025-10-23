module DFASpec (spec) where

import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import Regexle.DFA
import qualified Kleene.ERE as ERE
import Test.Syd

spec :: Spec
spec = describe "fromERE" $ do
  it "marks disallowed characters as dead alphabet" $ do
    let letters = ERE.unions (map ERE.char "NRQI")
        plus r = ERE.appends [r, ERE.star r]
        info = fromERE (plus letters)
        idxZ = expectIndex 'Z'
        idxN = expectIndex 'N'
    IntSet.member idxZ (diDeadAlphabet info) `shouldBe` True
    IntSet.member idxN (diDeadAlphabet info) `shouldBe` False

  it "records per-state dead-from sets" $ do
    let letters = ERE.unions (map ERE.char "NRQI")
        plus r = ERE.appends [r, ERE.star r]
        info = fromERE (plus letters)
        idxZ = expectIndex 'Z'
        deadFrom0 = diDeadFrom info V.! diInitial info
    IntSet.member idxZ deadFrom0 `shouldBe` True

expectIndex :: Char -> Int
expectIndex ch =
  case charToIndex ch of
    Just idx -> idx
    Nothing -> error $ "Character not in alphabet: " <> [ch]
