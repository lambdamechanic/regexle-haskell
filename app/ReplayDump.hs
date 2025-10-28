{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified Z3.Monad as Z3
import Regexle.Solver (enableZ3ConsistencyChecks)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

data Options = Options
  { optDumpDir :: !FilePath
  , optLimit :: !(Maybe Int)
  , optVerbose :: !Bool
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
        ( long "dump-dir"
       <> metavar "DIR"
       <> help "Directory containing base.smt2, driver.smt2, puzzle-*.smt2" )
  <*> optional
        ( option auto
            ( long "limit"
           <> metavar "INT"
           <> help "Replay only the first N puzzle files" ))
  <*> switch (long "verbose" <> help "Verbose logging")

--------------------------------------------------------------------------------
-- Command parsing
--------------------------------------------------------------------------------

data Command
  = CmdPush
  | CmdPop
  | CmdDeclareConst !Text !Text
  | CmdDeclareDatatypes ![(Text, [Text])]
  | CmdAssert !Text
  | CmdCheckSat
  | CmdGetModel
  | CmdGetValue ![Text]
  deriving (Show)

splitCommands :: Text -> [Text]
splitCommands input = go [] [] 0 False (T.unpack input)
  where
    go acc curr depth inComment []
      | depth /= 0 = error "Unbalanced parentheses in SMT2 input"
      | null curr = reverse acc
      | otherwise = error "Dangling partial command"
    go acc curr depth inComment (c:cs)
      | inComment && c == '\n' = go acc curr depth False cs
      | inComment = go acc curr depth True cs
      | depth == 0 && isSpace c = go acc curr depth False cs
      | c == ';' = go acc curr depth True cs
      | c == '(' = go acc ('(' : curr) (depth + 1) False cs
      | c == ')' =
          let depth' = depth - 1
              curr' = ')' : curr
           in if depth' == 0
                then let cmd = T.pack (reverse curr')
                      in go (cmd : acc) [] depth' False cs
                else go acc curr' depth' False cs
      | otherwise = go acc (c : curr) depth False cs

-- Simple s-expression representation

data SExpr = Atom Text | List [SExpr]
  deriving (Show, Eq)

tokenize :: Text -> [Text]
tokenize = go [] [] . T.unpack
  where
    go acc curr [] =
      let acc' = maybe acc (:acc) (emit curr)
       in reverse acc'
    go acc curr (ch:rest)
      | ch == '(' = go ("(":acc') [] rest
      | ch == ')' = go (")":acc') [] rest
      | isSpace ch = go acc' [] rest
      | otherwise = go acc (ch:curr) rest
      where
        acc' = maybe acc (:acc) (emit curr)
    emit [] = Nothing
    emit xs = Just (T.pack (reverse xs))

parseSExpr :: Text -> Either String SExpr
parseSExpr txt =
  case parseTokens (tokenize txt) of
    Left err -> Left err
    Right (sexpr, []) -> Right sexpr
    Right (_, leftover) -> Left ("Unexpected tokens: " <> show leftover)
  where
    parseTokens [] = Left "unexpected end of tokens"
    parseTokens (tok:toks)
      | tok == "(" = parseList toks
      | tok == ")" = Left "unexpected )"
      | otherwise = Right (Atom tok, toks)
    parseList [] = Left "missing closing )"
    parseList (tok:toks)
      | tok == ")" = Right (List [], toks)
      | otherwise = do
          (sexpr, rest) <- parseTokens (tok : toks)
          case parseList rest of
            Left err -> Left err
            Right (List xs, rest') -> Right (List (sexpr:xs), rest')

parseCommand :: Text -> Either String Command
parseCommand txt =
  case parseSExpr txt of
    Left err -> Left err
    Right sexpr -> case sexpr of
      List (Atom "push" : _) -> Right CmdPush
      List (Atom "pop" : _) -> Right CmdPop
      List [Atom "check-sat"] -> Right CmdCheckSat
      List [Atom "get-model"] -> Right CmdGetModel
      List (Atom "declare-const" : Atom name : Atom sortName : _) ->
        Right (CmdDeclareConst name sortName)
      List (Atom "declare-datatypes" : sortsSpec : ctorsSpec : _) ->
        Right (CmdDeclareDatatypes (extract sortsSpec ctorsSpec))
      List (Atom "assert" : _) -> Right (CmdAssert txt)
      List (Atom "get-value" : rest) -> Right (CmdGetValue (collect rest))
      _ -> Right (CmdAssert txt)
  where
    collect = mapMaybe $ \sexpr -> case sexpr of
      List [Atom name] -> Just name
      _ -> Nothing
    extract (List sortDecls) (List ctorDecls) =
      [ (sortName, map getCtorName ctors)
      | (List (Atom sortName : _), List ctors) <- zip sortDecls ctorDecls
      ]
    extract _ _ = []
    getCtorName (List (Atom ctor : _)) = ctor
    getCtorName (Atom ctor) = ctor
    getCtorName _ = error "Unsupported ctor form"
    mapMaybe f = foldr (maybe id (:) . f) []

--------------------------------------------------------------------------------
-- Harness state
--------------------------------------------------------------------------------

data HarnessState = HarnessState
  { hsSorts :: !(M.Map Text Z3.Sort)
  , hsDecls :: !(M.Map Text Z3.FuncDecl)
  , hsConstAsts :: !(M.Map Text Z3.AST)
  , hsCurrentModel :: !(Maybe Z3.Model)
  , hsVerbose :: !Bool
  }

initialState :: Bool -> HarnessState
initialState verbose = HarnessState
  { hsSorts = M.empty
  , hsDecls = M.empty
  , hsConstAsts = M.empty
  , hsCurrentModel = Nothing
  , hsVerbose = verbose
  }

z3 :: Z3.Z3 a -> StateT HarnessState Z3.Z3 a
z3 = lift

--------------------------------------------------------------------------------
-- Command execution
--------------------------------------------------------------------------------

runCommands :: [(FilePath, [Command])] -> StateT HarnessState Z3.Z3 ()
runCommands files = forM_ files $ \(path, commands) -> do
  v <- gets hsVerbose
  when v $ liftIO (putStrLn ("== " <> path))
  modify' $ \st -> st { hsCurrentModel = Nothing }
  forM_ commands execCommand
  where
    modify' f = modify f

execCommand :: Command -> StateT HarnessState Z3.Z3 ()
execCommand cmd =
  case cmd of
    CmdPush -> z3 Z3.solverPush
    CmdPop -> z3 (Z3.solverPop 1)
    CmdDeclareConst name sortName -> declareConst name sortName
    CmdDeclareDatatypes decls -> mapM_ declareDatatype decls
    CmdAssert txt -> addAssert txt
    CmdCheckSat -> do
      res <- z3 Z3.solverCheck
      when (res /= Z3.Sat) $ liftIO $ putStrLn ("solver returned " <> show res)
    CmdGetModel -> do
      model <- z3 Z3.solverGetModel
      modify $ \st -> st { hsCurrentModel = Just model }
    CmdGetValue names -> do
      mModel <- gets hsCurrentModel
      case mModel of
        Nothing -> pure ()
        Just model -> forM_ names $ \name -> do
          ast <- lookupConstAst name
          _ <- z3 (Z3.modelEval model ast True)
          pure ()

lookupConstAst :: Text -> StateT HarnessState Z3.Z3 Z3.AST
lookupConstAst name = do
  astMap <- gets hsConstAsts
  case M.lookup name astMap of
    Just ast -> pure ast
    Nothing -> do
      declMap <- gets hsDecls
      case M.lookup name declMap of
        Nothing -> error ("Unknown constant: " <> T.unpack name)
        Just decl -> do
          ast <- z3 $ Z3.mkApp decl []
          modify $ \st -> st { hsConstAsts = M.insert name ast (hsConstAsts st) }
          pure ast

addAssert :: Text -> StateT HarnessState Z3.Z3 ()
addAssert txt = do
  sortEntries <- M.toList <$> gets hsSorts
  declEntries <- M.toList <$> gets hsDecls
  let (sortNames, sortRefs) = unzip [(name, sort) | (name, sort) <- sortEntries]
      (declNames, declRefs) = unzip [(name, decl) | (name, decl) <- declEntries]
  sortSymbols <- mapM (z3 . Z3.mkStringSymbol . T.unpack) sortNames
  declSymbols <- mapM (z3 . Z3.mkStringSymbol . T.unpack) declNames
  ast <- z3 $ Z3.parseSMTLib2String (T.unpack txt) sortSymbols sortRefs declSymbols declRefs
  z3 $ Z3.solverAssertCnstr ast

declareDatatype :: (Text, [Text]) -> StateT HarnessState Z3.Z3 ()
declareDatatype (sortName, ctors) = do
  sym <- z3 $ Z3.mkStringSymbol (T.unpack sortName)
  constructors <- mapM mkCtor ctors
  sort <- z3 $ Z3.mkDatatype sym constructors
  modify $ \st -> st { hsSorts = M.insert sortName sort (hsSorts st) }
  where
    mkCtor ctorName = do
      ctorSym <- z3 $ Z3.mkStringSymbol (T.unpack ctorName)
      testerSym <- z3 $ Z3.mkStringSymbol (T.unpack (ctorName <> "?_"))
      z3 $ Z3.mkConstructor ctorSym testerSym []

declareConst :: Text -> Text -> StateT HarnessState Z3.Z3 ()
declareConst name sortName = do
  sortMap <- gets hsSorts
  case M.lookup sortName sortMap of
    Nothing -> error ("Unknown sort: " <> T.unpack sortName)
    Just sort -> do
      sym <- z3 $ Z3.mkStringSymbol (T.unpack name)
      ast <- z3 $ Z3.mkConst sym sort
      app <- z3 $ Z3.toApp ast
      decl <- z3 $ Z3.getAppDecl app
      modify $ \st -> st { hsDecls = M.insert name decl (hsDecls st)
                          , hsConstAsts = M.insert name ast (hsConstAsts st)
                          }

--------------------------------------------------------------------------------
-- File loading
--------------------------------------------------------------------------------

loadCommands :: Options -> IO [(FilePath, [Command])]
loadCommands Options { optDumpDir = dir, optLimit = mLimit } = do
  let basePath = dir </> "base.smt2"
  baseText <- TIO.readFile basePath
  let baseCmds = parseFileCommands basePath baseText
  entries <- Dir.listDirectory dir
  let puzzleFiles = sort [ entry | entry <- entries, "puzzle-" `T.isPrefixOf` T.pack entry ]
      limited = maybe puzzleFiles (`take` puzzleFiles) mLimit
  puzzleCmds <- mapM (\p -> do
      txt <- TIO.readFile (dir </> p)
      pure (dir </> p, parseFileCommands p txt)
    ) limited
  pure ((basePath, baseCmds) : puzzleCmds)

parseFileCommands :: FilePath -> Text -> [Command]
parseFileCommands ctx txt =
  let cmds = splitCommands txt
   in map (either (error . (ctx <>) . (": " <>)) id . parseCommand) cmds

--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

configureSolver :: Z3.Z3 ()
configureSolver = do
  params <- Z3.mkParams
  symAuto <- Z3.mkStringSymbol "auto_config"
  symSeed <- Z3.mkStringSymbol "random_seed"
  symPhase <- Z3.mkStringSymbol "phase_selection"
  symMacro <- Z3.mkStringSymbol "macro_finder"
  Z3.paramsSetBool params symAuto False
  Z3.paramsSetUInt params symSeed 7
  Z3.paramsSetUInt params symPhase 0
  Z3.paramsSetBool params symMacro True
  Z3.solverSetParams params

runReplay :: Options -> IO ()
runReplay opts = do
  enableZ3ConsistencyChecks
  files <- loadCommands opts
  let verbose = optVerbose opts
  _ <- Z3.evalZ3 $ do
    configureSolver
    evalStateT (runCommands files) (initialState verbose)
  pure ()

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Replay a hot-solver dump"))
  runReplay opts
