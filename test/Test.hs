{-# LANGUAGE LambdaCase #-}

import           Data.Char (isSpace)
import           Data.List (dropWhileEnd, isPrefixOf)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           System.Directory (listDirectory, doesDirectoryExist, doesFileExist, removeFile)
import           System.Exit (ExitCode(..))
import           System.FilePath ((<.>), (</>), takeBaseName, takeExtension)
import qualified System.Process as Proc
import           Text.Parsec (parse, (<|>), (<?>), string, many1, digit)
import           Text.Parsec.String (Parser)

import           Mir.SAWInterface (loadMIR, extractMIR)
import qualified Verifier.SAW.FiniteValue as FV
import qualified Verifier.SAW.Prelude as SC
import qualified Verifier.SAW.SCTypeCheck as SC
import qualified Verifier.SAW.SharedTerm as SC
import qualified Verifier.SAW.Simulator.Concrete as Conc

import           Test.Tasty (defaultMain, testGroup, TestTree)
import           Test.Tasty.HUnit (Assertion, testCaseSteps, assertBool, assertFailure)

generateMIR :: FilePath -> String -> IO ExitCode
generateMIR dir name = do
  (ec, _, _) <- Proc.readProcessWithExitCode "mir-json" [dir </> name <.> "rs", "--crate-type", "lib"] ""
  let rlibFile = ("lib" ++ name) <.> "rlib"
  doesFileExist rlibFile >>= \case
    True -> removeFile rlibFile
    False -> return ()
  return ec

compileAndRun :: FilePath -> String -> IO (Maybe String)
compileAndRun dir name = do
  (ec, _, _) <- Proc.readProcessWithExitCode "rustc" [dir </> name <.> "rs", "--cfg", "with_main"] ""
  case ec of
    ExitFailure _ -> do
      putStrLn $ "rustc compilation failed for " ++ name
      return Nothing
    ExitSuccess -> do
      let execFile = "." </> name
      (ec', out, _) <- Proc.readProcessWithExitCode execFile [] ""
      doesFileExist execFile >>= \case
        True -> removeFile execFile
        False -> return ()
      case ec' of
        ExitFailure _ -> do
          putStrLn $ "non-zero exit code for test executable " ++ name
          return Nothing
        ExitSuccess -> return $ Just out

oracleTest :: FilePath -> String -> (String -> IO ()) -> Assertion
oracleTest dir name step = do
  sc <- SC.mkSharedContext SC.preludeModule

  step "Compiling and running oracle program"
  oracleOut <- compileAndRun dir name >>= \case
    Nothing -> assertFailure "failed to compile and run"
    Just out -> return out

  step ("Oracle output: " ++ (dropWhileEnd isSpace oracleOut))

  step "Generating MIR JSON"
  generateMIR dir name >>= \case
    ExitFailure _ -> assertFailure "failed to generate mir-json"
    ExitSuccess -> return ()

  step "Loading MIR"
  mir <- loadMIR sc (dir </> name <.> "mir")
  step "Extracting function f"
  f <- extractMIR sc mir "f"
  step "Extracting argument ARG"
  arg <- extractMIR sc mir "ARG"

  step "Typechecking f(ARG)"
  app <- SC.scApply sc f arg
  rty <- SC.scTypeCheck sc app >>= \case
    Left e -> assertFailure $ "ill-typed result: " ++ show (SC.prettyTCError e)
    Right rty -> return rty
  ty <- FV.asFiniteType sc rty

  step "Parsing oracle output at inferred type"
  oracle <- case parse (parseRustFV ty) "oracleOut" oracleOut of
    Left e -> error $ "error parsing Rust output: " ++ show e
    Right fv -> FV.scFiniteValue sc fv

  step "Comparing oracle output"
  eq <- SC.scEq sc oracle app
  assertBool "oracle output mismatch"
    (Conc.toBool (Conc.evalSharedTerm (SC.scModule sc) Map.empty eq))

main :: IO ()
main = defaultMain =<< suite

suite :: IO TestTree
suite = testGroup "mir-verifier tests" <$> sequence
  [ testDir "test/conc_eval" ]

testDir :: FilePath -> IO TestTree
testDir dir = do
  putStrLn dir
  let gen f | "." `isPrefixOf` takeBaseName f = return Nothing
      gen f | takeExtension f == ".rs" = return (Just (testCaseSteps name (oracleTest dir name)))
        where name = (takeBaseName f)
      gen f = doesDirectoryExist f >>= \case
        False -> return Nothing
        True -> Just <$> testDir f
  fs <- listDirectory dir
  tcs <- mapM gen fs
  return (testGroup (takeBaseName dir) (catMaybes tcs))

-- | Parse the Rust program output into a finite value at a given type
parseRustFV :: FV.FiniteType -> Parser FV.FiniteValue
parseRustFV ft = case ft of
  FV.FTBit ->
    string "true" *> pure (FV.FVBit True)
    <|> string "false" *> pure (FV.FVBit False)
    <?> "boolean"
  FV.FTVec w FV.FTBit -> do
    i <- read <$> many1 digit
    return (FV.FVWord w i)
  FV.FTVec _n _elt -> error "unimplemented"
  FV.FTTuple _elts -> error "unimplemented"
  FV.FTRec _fields -> error "unimplemented"