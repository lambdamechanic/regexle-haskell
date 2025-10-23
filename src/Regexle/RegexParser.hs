{-# LANGUAGE OverloadedStrings #-}

module Regexle.RegexParser
  ( parseRegexToERE
  ) where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import qualified Kleene.ERE as ERE
import Text.Megaparsec
  ( Parsec
  , between
  , eof
  , many
  , optional
  , runParser
  , sepBy1
  , (<|>)
  )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC

type Parser = Parsec Void Text

data Regex
  = REmpty
  | REpsilon
  | RLiteral Char
  | RConcat [Regex]
  | RAlt [Regex]
  | RStar Regex
  | RPlus Regex
  | ROpt Regex
  | RClass Bool [ClassItem]
  | RAny
  deriving (Eq, Show)

data ClassItem
  = ClassRange Char Char
  | ClassChar Char
  deriving (Eq, Show)

parseRegexToERE :: Text -> Either String (ERE.ERE Char)
parseRegexToERE input =
  case runParser (regexP <* eof) "regex" input of
    Left err -> Left (MP.errorBundlePretty err)
    Right r -> Right (toERE r)

regexP :: Parser Regex
regexP = mkAlt <$> sepBy1 concatP (MPC.char '|')

concatP :: Parser Regex
concatP = mkConcat <$> MP.some repetitionP

repetitionP :: Parser Regex
repetitionP = do
  atom <- atomP
  quant <- optional quantifierP
  pure $
    case quant of
      Nothing -> atom
      Just q -> q atom

quantifierP :: Parser (Regex -> Regex)
quantifierP =
  MPC.char '*' $> RStar
    <|> MPC.char '+' $> RPlus
    <|> MPC.char '?' $> ROpt

atomP :: Parser Regex
atomP =
  literalP
    <|> dotP
    <|> classP
    <|> groupP

literalP :: Parser Regex
literalP = do
  c <- escapedChar <|> MP.satisfy (\ch -> ch `notElem` specialChars)
  pure (RLiteral c)
  where
    specialChars :: [Char]
    specialChars = "\\|().*+?[]"

escapedChar :: Parser Char
escapedChar = do
  _ <- MP.try (MPC.char '\\')
  MP.anySingle

dotP :: Parser Regex
dotP = MPC.char '.' $> RAny

groupP :: Parser Regex
groupP =
  between (MPC.char '(') (MPC.char ')') regexP

classP :: Parser Regex
classP = do
  _ <- MPC.char '['
  negated <- optional (MPC.char '^')
  items <- many classItemP
  _ <- MPC.char ']'
  pure $ RClass (negated == Just '^') items

classItemP :: Parser ClassItem
classItemP =
  MP.try rangeP <|> singleP
  where
    singleP = ClassChar <$> classChar

rangeP :: Parser ClassItem
rangeP = do
  start <- classChar
  _ <- MPC.char '-'
  end <- classChar
  pure (ClassRange start end)

classChar :: Parser Char
classChar =
  escapedChar
    <|> MP.satisfy (\c -> c /= ']' && c /= '-')

mkConcat :: [Regex] -> Regex
mkConcat [] = REpsilon
mkConcat [r] = r
mkConcat rs = RConcat rs

mkAlt :: [Regex] -> Regex
mkAlt [] = REmpty
mkAlt [r] = r
mkAlt rs = RAlt rs

toERE :: Regex -> ERE.ERE Char
toERE REmpty = ERE.empty
toERE REpsilon = ERE.eps
toERE (RLiteral c) = ERE.char c
toERE (RConcat rs) = ERE.appends (map toERE rs)
toERE (RAlt rs) = case rs of
  [] -> ERE.empty
  _ -> ERE.unions (map toERE rs)
toERE (RStar r) = ERE.star (toERE r)
toERE (RPlus r) =
  let ere = toERE r
   in ERE.appends [ere, ERE.star ere]
toERE (ROpt r) = ERE.unions [ERE.eps, toERE r]
toERE (RClass neg items) =
  let base = case map classItemToERE items of
        [] -> ERE.empty
        xs -> ERE.unions xs
   in if neg then ERE.complement base else base
toERE RAny = ERE.charRange 'A' 'Z'

classItemToERE :: ClassItem -> ERE.ERE Char
classItemToERE (ClassRange a b)
  | a <= b = ERE.charRange a b
  | otherwise = ERE.charRange b a
classItemToERE (ClassChar c) = ERE.char c
