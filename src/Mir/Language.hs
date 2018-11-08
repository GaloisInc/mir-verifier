{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Mir.Language (main) where

import qualified Data.Char       as Char
import           Data.Functor.Const (Const(..))
import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class
import qualified Data.List       as List
import qualified Data.Text       as Text
import           Data.Type.Equality ((:~:)(..),TestEquality(..))
import qualified Data.Map.Strict as Map
import qualified Data.Vector     as Vector
import qualified Text.Read       as Read

import           System.IO (stdout)
import           System.FilePath ((<.>), (</>), splitFileName,splitExtension)

import           Text.PrettyPrint.ANSI.Leijen (pretty)

import           Control.Lens((^.))

--import           GHC.Stack

-- parameterized-utils
import qualified Data.Parameterized.Map     as MapF
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.TraversableFC as Ctx

-- crucible
import qualified Lang.Crucible.Simulator               as C
import qualified Lang.Crucible.CFG.Core                as C
import qualified Lang.Crucible.FunctionHandle          as C
import qualified Lang.Crucible.Backend                 as C

-- what4
import qualified What4.Interface                       as W4
import qualified What4.Config                          as W4
import qualified What4.ProgramLoc                      as W4

-- crux
import qualified Crux.Language as Crux
import qualified Crux.CruxMain as Crux
--import qualified Crux.Error    as Crux
--import qualified Crux.Options  as Crux

import Crux.Types
--import Crux.Model
import Crux.Log

-- mir-verifier
import           Mir.Mir
import           Mir.PP()
import           Mir.Overrides
import           Mir.Intrinsics(MIR,mirExtImpl,cleanVariantName,parseFieldName)
import           Mir.SAWInterface (translateMIR,RustModule(..))
import           Mir.Generate(generateMIR)
import           Mir.Prims(loadPrims)

main :: IO ()
main = Crux.main [Crux.LangConf (Crux.defaultOptions @CruxMIR)]


data CruxMIR

instance Crux.Language CruxMIR where
  name = "mir"
  validExtensions = [".rs", ".rslib" ]

  type LangError CruxMIR = ()
  formatError  _ = ""

  data LangOptions CruxMIR = MIROptions
     {
     }
 
  defaultOptions = MIROptions
    {
    }

  envOptions = []

  simulate = simulateMIR

  makeCounterExamples = makeCounterExamplesMIR

simulateMIR :: forall sym. Crux.Simulate sym CruxMIR
simulateMIR  executeCrucible (cruxOpts, _mirOpts) sym p = do

  setSimulatorVerbosity (Crux.simVerbose cruxOpts) sym

  let filename      = Crux.inputFile cruxOpts
  let (dir,nameExt) = splitFileName filename
  let (name,_ext)   = splitExtension nameExt

  when (Crux.simVerbose cruxOpts > 2) $
    say "Crux" $ "Generating " ++ dir </> name <.> "mir"

  col1 <- generateMIR dir name

  prims <- liftIO $ loadPrims
  let col = prims <> col1

  when (Crux.simVerbose cruxOpts > 2) $ do
    say "Crux" $ "MIR collection"
    putStrLn $ show (pretty col1)

  res_ty <- case List.find (\fn -> fn^.fname == "::f[0]") (col^.functions) of
                   Just fn -> return (fn^.freturn_ty)
                   Nothing  -> fail "cannot find f"

  let mir = translateMIR col

  let cfgmap = rmCFGs mir

  let link :: C.OverrideSim p sym MIR rtp a r ()
      link   = forM_ (Map.toList cfgmap) $
                 \(fn, C.AnyCFG cfg) -> bindFn fn cfg


  (C.AnyCFG f_cfg) <- case (Map.lookup (Text.pack "::f[0]") cfgmap) of
                        Just c -> return c
                        _      -> fail $ "Could not find cfg: " ++ "f"
  (C.AnyCFG a_cfg) <- case (Map.lookup (Text.pack "::ARG[0]") cfgmap) of
                        Just c -> return c
                        _      -> fail $ "Could not find cfg: " ++ "g"

  when (Crux.simVerbose cruxOpts > 2) $ do
    say "Crux" "f CFG"
    print $ C.ppCFG True f_cfg
    say "Crux" "ARG CFG"
    print $ C.ppCFG True a_cfg

  let hf = C.cfgHandle f_cfg
  let ha = C.cfgHandle a_cfg

  Refl <- failIfNotEqual (C.handleArgTypes ha)   (W4.knownRepr :: C.CtxRepr Ctx.EmptyCtx)
         $ "Checking input to ARG"
  Refl <- failIfNotEqual (C.handleArgTypes hf) (Ctx.empty `Ctx.extend` C.handleReturnType ha)
         $ "Checking agreement between f and ARG"

  let
     osim :: Fun sym MIR Ctx.EmptyCtx C.UnitType
     osim   = do
        link
        arg <- C.callCFG a_cfg C.emptyRegMap
        res <- C.callCFG f_cfg (C.RegMap (Ctx.empty `Ctx.extend` arg))
        str <- showRegEntry @sym col res_ty res
        liftIO $ putStrLn $ str
        return ()

  halloc <- C.newHandleAllocator
  let simctx = C.initSimContext sym MapF.empty halloc stdout C.emptyHandleMap mirExtImpl p
      simst  = C.initSimState simctx C.emptyGlobals C.defaultAbortHandler

  res <- executeCrucible simst $ C.runOverrideSim (W4.knownRepr :: C.TypeRepr C.UnitType) osim
  return $ Result res


makeCounterExamplesMIR :: Crux.Options CruxMIR -> Maybe ProvedGoals -> IO ()
makeCounterExamplesMIR _opts = maybe (return ()) go
  where
    go gs =
      case gs of
        AtLoc _ _ gs1 -> go gs1
        Branch g1 g2 -> go g1 >> go g2
        Goal _ (c, _) _ res ->
          let _suff =
                case W4.plSourceLoc (C.simErrorLoc c) of
                  W4.SourcePos _ l _ -> show l
                  _                  -> "unknown"
              msg = show (C.simErrorReason c)
          in case res of
               NotProved (Just _m) ->
                 do sayFail "Crux" ("Failure for " ++ msg)
               _ -> return ()

-------------------------------------------------------
-- maybe add these to crux, as they are not specific to MIR?
failIfNotEqual :: forall f m a (b :: k).
                  (Monad m, Show (f a), Show (f b), TestEquality f)
               => f a -> f b -> String -> m (a :~: b)
failIfNotEqual r1 r2 str
  | Just Refl <- testEquality r1 r2 = return Refl
  | otherwise = fail $ str ++ ": mismatch between " ++ show r1 ++ " and " ++ show r2

setSimulatorVerbosity :: (W4.IsSymExprBuilder sym) => Int -> sym -> IO ()
setSimulatorVerbosity verbosity sym = do
  verbSetting <- W4.getOptionSetting W4.verbosity (W4.getConfiguration sym)
  _ <- W4.setOpt verbSetting (toInteger verbosity)
  return ()


showRegEntry :: forall sym arg p rtp args ret
   . C.IsSymInterface sym
  => Collection
  -> Ty
  -> C.RegEntry sym arg
  -> C.OverrideSim p sym MIR rtp args ret String
showRegEntry col mty (C.RegEntry tp rv) =
  case (mty,tp) of
    (TyBool, C.BoolRepr) -> return $ case W4.asConstantPred rv of
                     Just b -> if b then "true" else "false"
                     Nothing -> "Symbolic bool"
    (TyStr, C.StringRepr) -> return $ case W4.asString rv of
                     Just s -> show s
                     Nothing -> "Symbolic string"
    (TyInt USize, C.NatRepr) -> return $ case W4.asNat rv of
                     Just n -> show n
                     Nothing -> "Symbolic nat"
    (TyUint USize, C.NatRepr) -> return $ case W4.asNat rv of
                     Just n -> show n
                     Nothing -> "Symbolic nat"
    (TyInt _sz, C.BVRepr _w) -> return $ case W4.asSignedBV rv of
                     Just i  -> show i
                     Nothing -> "Symbolic BV"
    (TyUint _sz, C.BVRepr _w) -> return $ case W4.asUnsignedBV rv of
                     Just i  -> show i
                     Nothing -> "Symbolic BV"
    (TyFloat _,  C.RealValRepr) -> return $ case W4.asRational rv of
                     Just f -> show f
                     Nothing -> "Symbolic real"

    (TyTuple tys, C.StructRepr (ctxr :: C.CtxRepr ctx)) -> do
      let rv' :: Ctx.Assignment (C.RegValue' sym) ctx
          rv' = rv

      let
          go :: forall typ. Ctx.Index ctx typ -> C.RegValue' sym typ ->
                (C.OverrideSim p sym MIR rtp args ret (Const String typ))
          go idx (C.RV elt) = do
            let i   = Ctx.indexVal idx
            let mty0 = tys !! i
            let tp0  = ctxr Ctx.! idx
            str <- showRegEntry col mty0 (C.RegEntry tp0 elt)
            return (Const str)

      (cstrs :: Ctx.Assignment (Const String) ctx) <- Ctx.traverseWithIndex go rv'
      let strs = Ctx.toListFC (\(Const str) -> str) cstrs
      return $ "[" ++ List.intercalate ", " strs ++ "]"

    -- Tagged union type
    -- TODO: type arguments
    (TyAdt name _tyargs,
      C.StructRepr (Ctx.Empty Ctx.:> C.NatRepr Ctx.:> C.AnyRepr))
      | Just (Adt _ variants) <- List.find (\(Adt n _) -> name == n) (col^.adts) -> do
      let rv' :: Ctx.Assignment (C.RegValue' sym) (Ctx.EmptyCtx Ctx.::> C.NatType Ctx.::> C.AnyType)
          rv' = rv
      let kv = rv'  Ctx.! Ctx.i1of2
      case W4.asNat (C.unRV kv) of
        Just k  -> do
          let var = variants !! (fromInteger (toInteger k))
          case rv'  Ctx.! Ctx.i2of2 of
            (C.RV (C.AnyValue (C.StructRepr (ctxr :: C.CtxRepr ctx)) (av :: Ctx.Assignment (C.RegValue' sym) ctx))) -> do
              let goField :: forall typ. Ctx.Index ctx typ -> C.RegValue' sym typ
                          -> (C.OverrideSim p sym MIR rtp args ret (Const String typ))
                  goField idx (C.RV elt) = do
                    let (Field fName fty _fsubst) = (var^.vfields) !! (Ctx.indexVal idx)
                        cty0   = ctxr Ctx.! idx
                    str <- showRegEntry col fty (C.RegEntry cty0 elt)
                    case parseFieldName (Text.unpack (idText fName)) of
                      Just [_, fn] -> case Read.readMaybe fn of
                                        Just (_x :: Int) -> return $ (Const $ str)
                                        _  -> return $ (Const $ fn ++ ": " ++ str)
                      _       -> return $ (Const str)
              cstrs <- Ctx.traverseWithIndex goField av
              let strs = Ctx.toListFC (\(Const str) -> str) cstrs
              let body = List.intercalate ", " strs
              if Char.isDigit (head body) then
                return $ Text.unpack (cleanVariantName (var^.vname)) ++ "(" ++ body  ++ ")"
              else
                return $ Text.unpack (cleanVariantName (var^.vname)) ++ " { " ++ body ++ " }"
            _ -> fail "invalide representation of ADT"
        Nothing -> return $ "Symbolic ADT:" ++ show name

    (TyRef ty Immut, _) -> showRegEntry col ty (C.RegEntry tp rv)

    (TyArray ty _sz, C.VectorRepr tyr) -> do
      -- rv is a Vector (RegValue tyr)
      let entries = Vector.map (C.RegEntry tyr) rv
      values <- Vector.mapM (showRegEntry col ty) entries
      let strs = Vector.toList values
      return $ "[" ++ List.intercalate ", " strs ++ "]"


    _ -> return $ "I don't know how to print result of type " ++ show (pretty mty)


-----------------------