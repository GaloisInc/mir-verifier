{- |
Module           : Mir.JSON
Description      : JSON to Mir AST parser
License          : BSD3
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mir.JSON where

import Data.Aeson
import qualified Data.Aeson.Types  as Aeson
import qualified Data.HashMap.Lazy as HML
import qualified Data.Map.Strict   as Map
import qualified Data.Scientific   as Scientific

import Data.Word(Word64)
import Data.Bits
import qualified Data.Char as Char
import Data.Text (Text,  unpack)
import qualified Data.Text as T
import qualified Data.Text.Read  as T
import Data.List
import qualified Data.Vector as V
import Control.Lens((^.),(&),(.~))

import Mir.DefId 
import Mir.Mir

import Debug.Trace

--------------------------------------------------------------------------------------
-- | FromJSON instances

-- Aeson is used for JSON deserialization


instance FromJSON BaseSize where
    parseJSON = withObject "BaseSize" $
                \t -> case HML.lookup "kind" t of
                        Just (String "usize") -> pure USize
                        Just (String "u8") -> pure B8
                        Just (String "u16") -> pure B16
                        Just (String "u32") -> pure B32
                        Just (String "u64") -> pure B64
                        Just (String "u128") -> pure B128
                        Just (String "isize") -> pure USize
                        Just (String "i8") -> pure B8
                        Just (String "i16") -> pure B16
                        Just (String "i32") -> pure B32
                        Just (String "i64") -> pure B64
                        Just (String "i128") -> pure B128
                        sz -> fail $ "unknown base size: " ++ show sz

instance FromJSON FloatKind where
    parseJSON = withObject "FloatKind" $ \t -> case HML.lookup "kind" t of
                                                 Just (String "f32") -> pure F32
                                                 Just (String "f64") -> pure F64
                                                 sz -> fail $ "unknown float type: " ++ show sz

instance FromJSON Substs where
    parseJSON v = do
      ml <- parseJSONList v
      case sequence ml of
        Just l  -> pure $ Substs l
        Nothing -> fail "invalid type argument found in substs"

instance FromJSON Ty where
    parseJSON = withObject "Ty" $ \v -> case HML.lookup "kind" v of
                                          Just (String "Bool") -> pure TyBool
                                          Just (String "Char") -> pure TyChar
                                          Just (String "Int") -> TyInt <$> v .: "intkind"
                                          Just (String "Uint") -> TyUint <$> v .: "uintkind"
                                          Just (String "Unsupported") -> pure TyUnsupported
                                          Just (String "Tuple") -> TyTuple <$> v .: "tys"
                                          Just (String "Slice") -> TySlice <$> v .: "ty"
                                          Just (String "Array") -> do
                                            lit <- v .: "size"
                                            case lit of
                                              Value (ConstInt (Usize i)) ->
                                                 TyArray <$> v .: "ty" <*> pure (fromInteger i)
                                              _ -> fail $ "unsupported array size: " ++ show lit
                                          Just (String "Ref") ->  TyRef <$> v .: "ty" <*> v .: "mutability"
                                          Just (String "Custom") -> TyCustom <$> v .: "data"
                                          Just (String "FnDef") -> TyFnDef <$> v .: "defid" <*> v .: "substs"
                                          Just (String "Adt") -> TyAdt <$> v .: "name" <*> v .: "substs"
                                          Just (String "Param") -> TyParam <$> v .: "param"
                                          Just (String "Closure") -> TyClosure <$> v .: "upvar_tys"
                                          Just (String "Str") -> pure TyStr
                                          Just (String "FnPtr") -> TyFnPtr <$> v .: "signature"
                                          Just (String "Dynamic") -> TyDynamic <$>
                                                (v .: "predicates" >>= \xs -> mapM parsePred xs)
                                          Just (String "RawPtr") -> TyRawPtr <$> v .: "ty" <*> v .: "mutability"
                                          Just (String "Float") -> TyFloat <$> v .: "size"
                                          Just (String "Never") -> pure TyNever
                                          Just (String "Projection") -> TyProjection <$> v .: "defid" <*> v .: "substs"
                                          Just (String "Lifetime") -> pure TyLifetime
                                          Just (String "Const") -> pure TyConst
                                          r -> fail $ "unsupported ty: " ++ show r

instance FromJSON Instance where
    parseJSON = withObject "Instance" $ \v -> case HML.lookup "kind" v of
        Just (String "Item") -> Instance IkItem
            <$> v .: "def_id" <*> v .: "substs"
        Just (String "Intrinsic") -> Instance IkIntrinsic
            <$> v .: "def_id" <*> v .: "substs"
        Just (String "VtableShim") -> Instance IkVtableShim
            <$> v .: "def_id" <*> v .: "substs"
        Just (String "FnPtrShim") -> Instance
            <$> (IkFnPtrShim <$> v .: "ty") <*> v .: "def_id" <*> v .: "substs"
        Just (String "Virtual") -> Instance
            <$> (IkVirtual <$> v .: "index") <*> v .: "def_id" <*> v .: "substs"
        Just (String "ClosureOnceShim") -> Instance IkClosureOnceShim
            <$> v .: "call_once" <*> v .: "substs"
        Just (String "DropGlue") -> Instance
            <$> (IkDropGlue <$> v .: "ty") <*> v .: "def_id" <*> v .: "substs"
        Just (String "CloneShim") -> Instance
            <$> (IkCloneShim <$> v .: "ty") <*> v .: "def_id" <*> v .: "substs"

instance FromJSON FnSig where
    parseJSON =
      withObject "FnSig" $ \v -> do
         let gens  = return []
         let preds = return []
         let atys  = return []
         FnSig <$> v .: "inputs"
               <*> v .: "output"
               <*> gens
               <*> preds
               <*> atys
               <*> v .: "abi"
               
instance FromJSON Adt where
    parseJSON = withObject "Adt" $ \v -> Adt <$> v .: "name" <*> v .: "kind" <*> v .: "variants"

instance FromJSON AdtKind where
    parseJSON x = case x of
        String "Struct" -> pure Struct
        String "Enum" -> pure Enum
        String "Union" -> pure Union
        _ -> fail $ "unsupported adt kind " ++ show x

instance FromJSON VariantDiscr where
    parseJSON = withObject "VariantDiscr" $ \v -> case HML.lookup "kind" v of
                                                    Just (String "Explicit") -> Explicit <$> v .: "name"
                                                    Just (String "Relative") -> Relative <$> v .: "index"
                                                    _ -> fail "unspported variant discriminator"

instance FromJSON CtorKind where
    parseJSON = withObject "CtorKind" $ \v -> case HML.lookup "kind" v of
                                                Just (String "Fn") -> pure FnKind
                                                Just (String "Const") -> pure ConstKind
                                                Just (String "Fictive") -> pure FictiveKind
                                                _ -> fail "unspported constructor kind"
instance FromJSON Variant where
    parseJSON = withObject "Variant" $ \v -> Variant <$> v .: "name" <*> v .: "discr" <*> v .: "fields" <*> v .: "ctor_kind"

instance FromJSON Field where
    parseJSON = withObject "Field" $ \v -> Field <$> v .: "name" <*> v .: "ty" <*> v .: "substs"

instance FromJSON CustomTy where
    parseJSON = withObject "CustomTy" $ \v -> case HML.lookup "kind" v of
                                                Just (String "Box") -> BoxTy <$> v .: "box_ty"
                                                Just (String "Vec") -> VecTy <$> v .: "vec_ty"
                                                Just (String "Iter") -> IterTy <$> v .: "iter_ty"
                                                x -> fail $ "bad custom type: " ++ show x

instance FromJSON Mutability where
    parseJSON = withObject "Mutability" $ \v -> case HML.lookup "kind" v of
                                                Just (String "MutMutable") -> pure Mut
                                                Just (String "Mut") -> pure Mut
                                                Just (String "MutImmutable") -> pure Immut
                                                Just (String "Not") -> pure Immut
                                                x -> fail $ "bad mutability: " ++ show x


instance FromJSON Var where
    parseJSON = withObject "Var" $ \v -> Var
        <$>  v .: "name"
        <*>  v .: "mut"
        <*>  v .: "ty"
        <*>  v .: "is_zst"
        <*>  v .: "scope"
        <*>  v .: "pos"

instance FromJSON Collection where
    parseJSON = withObject "Collection" $ \v -> do
      (fns    :: [Fn])        <- v .: "fns"
      (adts   :: [Adt])       <- v .: "adts"
      (traits :: [Trait])     <- v .: "traits"
      (impls  :: [TraitImpl]) <- v .: "impls"
      (statics :: [Static]  ) <- v .: "statics"
      (vtables :: [Vtable]  ) <- v .: "vtables"
      (intrinsics :: [Intrinsic]) <- v .: "intrinsics"
      (roots :: [MethName])   <- v .: "roots"
      return $ Collection
        (foldr (\ x m -> Map.insert (x^.fname) x m)     Map.empty fns)
        (foldr (\ x m -> Map.insert (x^.adtname) x m)   Map.empty adts)
        (foldr (\ x m -> Map.insert (x^.traitName) x m) Map.empty traits)
        impls
        (foldr (\ x m -> Map.insert (x^.sName) x m)     Map.empty statics)
        (foldr (\ x m -> Map.insert (x^.vtName) x m)    Map.empty vtables)
        (foldr (\ x m -> Map.insert (x^.intrName) x m)  Map.empty intrinsics)
        roots


instance FromJSON Fn where
    parseJSON = withObject "Fn" $ \v -> do
      pg <- v .: "generics"
      pp <- v .: "predicates"
      args <- v .: "args"
      let sig = FnSig <$> return (map typeOf args)
                      <*> v .: "return_ty"
                      <*> (withObject "Param" (\u -> u .: "params") pg)
                      <*> (withObject "Predicates" (\u -> u .: "predicates") pp)
                      <*> return []
                      <*> v .: "abi"

      Fn
        <$> v .: "name"
        <*> return args
        <*> sig        
        <*> v .: "body"
        <*> v .: "promoted"

instance FromJSON Abi where
    parseJSON = withObject "Abi" $ \v -> case HML.lookup "kind" v of
        Just (String "Rust") -> pure RustAbi
        Just (String "RustIntrinsic") -> pure RustIntrinsic
        Just (String "RustCall") -> pure RustCall
        Just (String _) -> pure OtherAbi
        x -> fail $ "bad abi: " ++ show x
        
instance FromJSON BasicBlock where
    parseJSON = withObject "BasicBlock" $ \v -> BasicBlock
        <$> v .: "blockid"
        <*> v .: "block"

instance FromJSON BasicBlockData where
    parseJSON = withObject "BasicBlockData" $ \v -> BasicBlockData
        <$> v .: "data"
        <*> v .: "terminator"

instance FromJSON Statement where
    parseJSON = withObject "Statement" $ \v -> case HML.lookup "kind" v of
                             Just (String "Assign") ->  Assign <$> v.: "lhs" <*> v .: "rhs" <*> v .: "pos"
                             Just (String "SetDiscriminant") -> SetDiscriminant <$> v .: "lvalue" <*> v .: "variant_index"
                             Just (String "StorageLive") -> StorageLive <$> v .: "slvar"
                             Just (String "StorageDead") -> StorageDead <$> v .: "sdvar"
                             Just (String "Nop") -> pure Nop
                             k -> fail $ "kind not found for statement: " ++ show k


data RustcPlace = RustcPlace PlaceBase (Maybe RustcProjection)
data RustcProjection = RustcProjection (Maybe RustcProjection) PlaceElem

instance FromJSON RustcPlace where
    parseJSON = withObject "Place" $ \v ->
        RustcPlace <$> v .: "base" <*> v .: "data"

instance FromJSON RustcProjection where
    parseJSON = withObject "Projection" $ \v ->
        RustcProjection <$> v .: "base" <*> v .: "data"

instance FromJSON PlaceBase where
    parseJSON = withObject "PlaceBase" $ \v ->
      case HML.lookup "kind" v of
        Just (String "Local")      -> Local       <$> v .: "localvar"
        Just (String "Static")     -> PStatic     <$> v .: "def_id" <*> v .: "ty"
        Just (String "Promoted")   -> PPromoted   <$> v .: "index" <*> v .: "ty"
        k -> fail $ "kind not found for PlaceBase " ++ show k

instance FromJSON PlaceElem where
    parseJSON = withObject "Lvpelem" $ \v ->
      case HML.lookup "kind" v of
        Just (String "Deref") -> pure Deref
        Just (String "Field") -> PField <$> v .: "field" <*> v .: "ty"
        Just (String "Index") -> Index <$> v .: "op"
        Just (String "ConstantIndex") -> ConstantIndex <$> v .: "offset" <*> v .: "min_length" <*> v .: "from_end"
        Just (String "Subslice") -> Subslice <$> v .: "from" <*> v .: "to"
        Just (String "Downcast") -> Downcast <$> v .: "variant"
        x -> fail ("bad lvpelem: " ++ show x)

instance FromJSON Lvalue where
    parseJSON j = convert <$> parseJSON j
      where
        convert (RustcPlace base Nothing) = LBase base
        convert (RustcPlace base (Just proj)) = convertProj (LBase base) proj

        convertProj base (RustcProjection Nothing elem) = LProj base elem
        convertProj base (RustcProjection (Just proj') elem) =
            LProj (convertProj base proj') elem

instance FromJSON Promoted where
    parseJSON = withScientific "Promoted" $ \sci ->
                  case Scientific.toBoundedInteger sci of
                    Just b  -> pure (Promoted b)
                    Nothing -> fail $ "cannot read " ++ show sci

instance FromJSON Rvalue where
    parseJSON = withObject "Rvalue" $ \v -> case HML.lookup "kind" v of
                                              Just (String "Use") -> Use <$> v .: "usevar"
                                              Just (String "Repeat") -> Repeat <$> v .: "op" <*> v .: "len"
                                              Just (String "Ref") ->  Ref <$> v .: "borrowkind" <*> v .: "refvar" <*> v .: "region"
                                              Just (String "Len") -> Len <$> v .: "lv"
                                              Just (String "Cast") -> Cast <$> v .: "type" <*> v .: "op" <*> v .: "ty"
                                              Just (String "BinaryOp") -> BinaryOp <$> v .: "op" <*> v .: "L" <*> v .: "R"
                                              Just (String "CheckedBinaryOp") -> CheckedBinaryOp <$> v .: "op" <*> v .: "L" <*> v .: "R"
                                              Just (String "NullaryOp") -> NullaryOp <$> v .: "op" <*> v .: "ty"
                                              Just (String "UnaryOp") -> UnaryOp <$> v .: "uop" <*> v .: "op"
                                              Just (String "Discriminant") -> Discriminant <$> v .: "val"
                                              Just (String "Aggregate") -> Aggregate <$> v .: "akind" <*> v .: "ops"
                                              Just (String "Custom") -> RCustom <$> v .: "data"
                                              k -> fail $ "unsupported RValue " ++ show k

instance FromJSON Terminator where
    parseJSON = withObject "Terminator" $ \v -> case HML.lookup "kind" v of
                                                  Just (String "Goto") -> Goto <$> v .: "target"
                                                  Just (String "SwitchInt") ->
                                                    let  q :: Aeson.Parser [Maybe Integer]
                                                         q = do
                                                               lmt <- v .: "values"
                                                               mapM (mapM convertIntegerText) lmt
                                                    in
                                                    SwitchInt <$> v .: "discr" <*> v .: "switch_ty" <*> q <*> v .: "targets"
                                                  Just (String "Resume") -> pure Resume
                                                  Just (String "Return") -> pure Return
                                                  Just (String "Unreachable") -> pure Unreachable
                                                  Just (String "Drop") -> Drop <$> v .: "location" <*> v .: "target" <*> v .: "unwind"
                                                  Just (String "DropAndReplace") -> DropAndReplace <$> v .: "location" <*> v .: "value" <*> v .: "target" <*> v .: "unwind"
                                                  Just (String "Call") ->  Call <$> v .: "func" <*> v .: "args" <*> v .: "destination" <*> v .: "cleanup"
                                                  Just (String "Assert") -> Assert <$> v .: "cond" <*> v .: "expected" <*> v .: "msg" <*> v .: "target" <*> v .: "cleanup"
                                                  k -> fail $ "unsupported terminator" ++ show k

instance FromJSON Operand where
    parseJSON = withObject "Operand" $ \v -> case HML.lookup "kind" v of
                                               Just (String "Move") -> Move <$> v .: "data"
                                               Just (String "Copy") -> Copy <$> v .: "data"  
                                               Just (String "Constant") -> OpConstant <$> v .: "data"
                                               x -> fail ("base operand: " ++ show x)

instance FromJSON Constant where
    parseJSON = withObject "Constant" $ \v -> Constant <$> v .: "ty" <*> v .: "literal"
    
instance FromJSON NullOp where
    parseJSON = withObject "NullOp" $ \v -> case HML.lookup "kind" v of
                                             Just (String "SizeOf") -> pure SizeOf
                                             Just (String "Box") -> pure Box
                                             x -> fail ("bad nullOp: " ++ show x)

instance FromJSON BorrowKind where
    parseJSON = withText "BorrowKind" $ \t ->
           if t == "Shared" then pure Shared
      else if t == "Unique" then pure Unique
      else if t == "Mut"    then pure Mutable
      -- s can be followed by "{ allow_two_phase_borrow: true }"
      else if T.isPrefixOf "Mut" t then pure Mutable
      else fail ("bad borrowKind: " ++ show t) 
       


      
instance FromJSON UnOp where
    parseJSON = withObject "UnOp" $ \v -> case HML.lookup "kind" v of
                                             Just (String "Not") -> pure Not
                                             Just (String "Neg") -> pure Neg
                                             x -> fail ("bad unOp: " ++ show x)
instance FromJSON BinOp where
    parseJSON = withObject "BinOp" $ \v -> case HML.lookup "kind" v of
                                             Just (String "Add") -> pure Add
                                             Just (String "Sub") -> pure Sub
                                             Just (String "Mul") -> pure Mul
                                             Just (String "Div") -> pure Div
                                             Just (String "Rem") -> pure Rem
                                             Just (String "BitXor") -> pure BitXor
                                             Just (String "BitAnd") -> pure BitAnd
                                             Just (String "BitOr") -> pure BitOr
                                             Just (String "Shl") -> pure Shl
                                             Just (String "Shr") -> pure Shr
                                             Just (String "Eq") -> pure Beq
                                             Just (String "Lt") -> pure Lt
                                             Just (String "Le") -> pure Le
                                             Just (String "Ne") -> pure Ne
                                             Just (String "Ge") -> pure Ge
                                             Just (String "Gt") -> pure Gt
                                             Just (String "Offset") -> pure Offset
                                             x -> fail ("bad binop: " ++ show x)


instance FromJSON VtableItem where
    parseJSON = withObject "VtableItem" $ \v ->
        VtableItem <$> v .: "def_id" <*> (_inDefId <$> v .: "instance")

instance FromJSON Vtable where
    parseJSON = withObject "Vtable" $ \v ->
        Vtable <$> v .: "name" <*> v .: "items"

instance FromJSON CastKind where
    parseJSON = withObject "CastKind" $ \v -> case HML.lookup "kind" v of
                                               Just (String "Misc") -> pure Misc
                                               Just (String "Pointer(ReifyFnPointer)") -> pure ReifyFnPointer
                                               Just (String "Pointer(ClosureFnPointer)") -> pure ClosureFnPointer
                                               Just (String "Pointer(UnsafeFnPointer)") -> pure UnsafeFnPointer
                                               Just (String "Pointer(Unsize)") -> pure Unsize
                                               Just (String "Pointer(MutToConstPointer)") -> pure MutToConstPointer
                                               Just (String "UnsizeVtable") -> UnsizeVtable <$> v .: "vtable"
                                               x -> fail ("bad CastKind: " ++ show x)

instance FromJSON Literal where
    parseJSON = withObject "Literal" $ \v ->
      case HML.lookup "kind" v of
        Just (String "Item") -> Item <$> v .: "def_id" <*> v .: "substs"
        Just (String "Const") -> do
          ty  <- v.: "ty"
          lit <- parseConst ty v
          pure $ Value lit
        Just (String "Promoted") -> LitPromoted <$> v .: "index"
        x -> fail ("bad Literal: " ++ show x)


-- | Need to look at both the val and the ty objects to figure out
-- how to parse the constant
parseConst :: Ty -> HML.HashMap Text Value -> Aeson.Parser ConstVal
parseConst ty v = do
  case ty of
    TyInt _bs  -> (v .: "int_val") >>= \t -> ConstInt <$> (convertInt ty t)
    TyUint _bs -> (v .: "int_val") >>= \t -> ConstInt <$> (convertInt ty t)
    TyFloat fk -> (v .: "float_val") >>= \t -> ConstFloat <$> (convertFloat fk t)
    TyBool     -> (v .: "int_val") >>= \t -> ConstBool <$> convertBool t
    TyChar     -> (v .: "int_val") >>= \t -> ConstChar <$> convertChar t
    TyRef t Immut -> parseConst t v
    TyStr        -> (v .: "str_val") >>= \t -> ConstStr <$> convertString t
    TyFnDef d ps -> pure $ ConstFunction d ps
    TyTuple ts   -> (v .: "initializer") >>= parseInitializer
    TyArray t n  -> parseArray t n v
    TyAdt d ss   -> (v .: "initializer") >>= parseInitializer
    r            -> fail $ "TODO: Don't know how to parse literals of type " ++ show ty
                             ++ "\nin JSON value " ++ show v

parseArray :: Ty -> Int -> HML.HashMap Text Value -> Aeson.Parser ConstVal
parseArray t n v =
  do mvec  <- v .:? "bstr_val"
     minit <- v .:? "initializer"
     case mvec of
       Just (arr :: Array) -> do
         let f sci = case Scientific.toBoundedInteger sci of
                       Just b -> pure (ConstInt $ U8 $ toInteger (b :: Int))
                       Nothing -> fail $ "cannot read " ++ show sci
         bytes <- mapM (withScientific "byte" f) arr
         return $ ConstArray $ V.toList bytes

       Nothing -> case minit of
         Just init -> parseInitializer init
         Nothing -> fail "either need bstr_val or initializer for array literals"
   

-- | Constant value from initializer code
parseInitializer :: Value -> Aeson.Parser ConstVal
parseInitializer = withObject "Initializer" $ \v ->
  ConstInitializer <$> v .: "def_id" <*> v.: "substs"

-- mir-json integers are expressed as strings of 128-bit unsigned values
-- for example, -1 is displayed as "18446744073709551615"
-- we need to parse this as a 128 unsigned bit Int value and then
-- cast it to a signed value
convertIntegerText :: Text -> Aeson.Parser Integer
convertIntegerText t = do
  case (T.signed T.decimal) t of
    Right ((i :: Word64), _) ->
      if testBit i 63 then do
        let ans = -1 * (toInteger (complement i + 1)) 
        return $ ans
      else do
        return $ toInteger i
    Left _       -> fail $ "Cannot parse Integer value: " ++ T.unpack t
    
convertInt :: Ty -> Text -> Aeson.Parser IntLit
convertInt ty val = 
  case ty of
    (TyUint B8)    -> U8    <$> convertIntegerText val
    (TyUint B16)   -> U16   <$> convertIntegerText val
    (TyUint B32)   -> U32   <$> convertIntegerText val
    (TyUint B64)   -> U64   <$> convertIntegerText val
    (TyUint B128)  -> U128  <$> convertIntegerText val
    (TyUint USize) -> Usize <$> convertIntegerText val
    (TyInt B8)     -> I8    <$> convertIntegerText val
    (TyInt B16)    -> I16   <$> convertIntegerText val
    (TyInt B32)    -> I32   <$> convertIntegerText val
    (TyInt B64)    -> I64   <$> convertIntegerText val
    (TyInt B128)   -> I128  <$> convertIntegerText val
    (TyInt USize)  -> Isize <$> convertIntegerText val
    _ -> fail "invalid int literal"

convertBool :: Text -> Aeson.Parser Bool
convertBool txt = if txt == "1" then pure True else if txt == "0" then pure False else fail $ "Cannot parse boolean value " ++ (T.unpack txt)

convertChar :: Text -> Aeson.Parser Char
convertChar txt = do
  i <- convertIntegerText txt
  return $ Char.chr (fromInteger i)

convertFloat :: FloatKind -> Text -> Aeson.Parser FloatLit
convertFloat fk val = 
    FloatLit <$> pure fk <*> pure (T.unpack val)
  
convertString :: Array -> Aeson.Parser String
convertString arr = do
    let f sci = case Scientific.toBoundedInteger sci of
                   Just b -> pure (Char.chr b)
                   Nothing -> fail $ "cannot read " ++ show sci
    bytes <- mapM (withScientific "byte" f) arr
    return $ V.toList bytes

{-

-- | Need to look at both the val and the ty objects to figure out
-- how to parse the constant
parseConst :: Ty -> Value -> Aeson.Parser ConstVal
parseConst ty v = do
  case ty of
    TyInt _bs  -> ConstInt   <$> parseInt ty v
    TyUint _bs -> ConstInt   <$> parseInt ty v
    TyFloat fk -> ConstFloat <$> parseFloat fk v
    TyBool     -> ConstBool  <$> parseBoolText v
    TyChar     -> ConstChar  <$> parseChar v
    TyRef t Immut -> parseConst t v
    TyStr      -> ConstStr   <$> parseJSON v
    TyFnDef d ps -> pure $ ConstFunction d ps   -- TODO : don't need v?
    TyTuple ts   -> ConstTuple <$> parseConsts ts v
    TyArray t n  -> ConstArray <$> parseConsts (replicate n t)  v
    r            -> fail $ "TODO: don't know how to parse literals of type " ++ show r


-- FromJSON instance for booleans assumes a "Bool" variant of the Value datatype
-- not a "String" variant
parseBoolText :: Value -> Aeson.Parser Bool
parseBoolText = withText "Bool" $ \t -> case t of
        "true"  -> pure True
        "false" -> pure False
        _       -> fail $ "Cannot parse key into Bool: " ++ T.unpack t

parseIntegerText :: Value -> Aeson.Parser Integer
parseIntegerText = withText "Integer" convertIntegerText

parseChar :: Value -> Aeson.Parser Char
parseChar = withText "Char" $ \t ->
  case Char.readLitChar (T.unpack t) of
    (c , _):_  -> return c
    _          -> fail $ "Don't know how to parse Text " ++ T.unpack t ++ " into a Char"

parseString :: Value -> Aeson.Parser String
parseString = withText "String" (pure . T.unpack)

parseArray :: Ty -> Value -> Aeson.Parser [ConstVal]
parseArray ty = withArray "expecting array"
  (\array -> V.foldM go [] array) where
     go :: [ConstVal] -> Value -> Aeson.Parser [ConstVal]
     go vals val = do
       v <- parseConst ty val
       return (v : vals)

parseConsts :: [Ty] -> Value -> Aeson.Parser [ConstVal]
parseConsts tys (String txt) = fail $ "TODO: parse consts " ++ show txt

parseInt :: Ty -> Value -> Aeson.Parser IntLit
parseInt ty val = 
  case ty of
    (TyUint B8)    -> U8    <$> parseIntegerText val
    (TyUint B16)   -> U16   <$> parseIntegerText val
    (TyUint B32)   -> U32   <$> parseIntegerText val
    (TyUint B64)   -> U64   <$> parseIntegerText val
    (TyUint B128)  -> U128  <$> parseIntegerText val
    (TyUint USize) -> Usize <$> parseIntegerText val
    (TyInt B8)     -> I8    <$> parseIntegerText val
    (TyInt B16)    -> I16   <$> parseIntegerText val
    (TyInt B32)    -> I32   <$> parseIntegerText val
    (TyInt B64)    -> I64   <$> parseIntegerText val
    (TyInt B128)   -> I128  <$> parseIntegerText val
    (TyInt USize)  -> Isize <$> parseIntegerText val
    _ -> fail "invalid int literal"

parseFloat :: FloatKind -> Value -> Aeson.Parser FloatLit
parseFloat fk val = 
    FloatLit <$> pure fk <*> parseJSON val

-}

instance FromJSON FloatLit where
    parseJSON = withObject "FloatLit" $ \v -> FloatLit <$> v .: "ty" <*> v.: "bits"

instance FromJSON AggregateKind where
    parseJSON = withObject "AggregateKind" $ \v -> case HML.lookup "kind" v of
                                                     Just (String "Array") -> AKArray <$> v .: "ty"
                                                     Just (String "Tuple") -> pure AKTuple
                                                     Just (String "Closure") -> AKClosure <$> v .: "defid" <*> v .: "closuresubsts"
                                                     Just (String unk) -> fail $ "unimp: " ++ unpack unk
                                                     x -> fail ("bad AggregateKind: " ++ show x)

instance FromJSON CustomAggregate where
    parseJSON = withObject "CustomAggregate" $ \v -> case HML.lookup "kind" v of
                                                       Just (String "Range") -> CARange <$> v .: "range_ty" <*> v .: "f1" <*> v .: "f2"
                                                       x -> fail ("bad CustomAggregate: " ++ show x)

instance FromJSON Trait where
    parseJSON = withObject "Trait" $ \v -> do
      pg <- v .: "generics"
      pp <- v .: "predicates"
      params <- (withObject "Param" (\u -> u .: "params") pg)
      Trait <$> v .: "name"
            <*> v .: "items"
            <*> v .: "supertraits"
            <*> pure params
            <*> (withObject "Predicates" (\u -> u .: "predicates") pp)
            <*> pure []

instance FromJSON TraitItem where
    parseJSON = withObject "TraitItem" $ \v ->
                case HML.lookup "kind" v of
                  Just (String "Method") -> do
                    sig <- v .: "signature"
                    pg  <- v .: "generics"
                    pp  <- v .: "predicates"
                    params <- withObject "Param" (\u -> u .: "params") pg
                    preds  <- withObject "Predicates" (\u -> u .: "predicates") pp
                    let sig' = sig & fsgenerics   .~ params
                                   & fspredicates .~ preds
                    TraitMethod <$> v .: "name" <*> return sig'
                  Just (String "Type") -> TraitType <$> v .: "name"
                  Just (String "Const") -> TraitConst <$> v .: "name" <*> v .: "type"
                  Just (String unk) -> fail $ "unknown trait item type: " ++ unpack unk
                  Just x -> fail $ "Incorrect format of the kind field in TraitItem: " ++ show x
                  k -> fail $ "bad kind field in TraitItem " ++ show k


instance FromJSON Intrinsic where
    parseJSON = withObject "Intrinsic" $ \v ->
        Intrinsic <$> v .: "name" <*> v .: "inst"


instance FromJSON MirBody where
    parseJSON = withObject "MirBody" $ \v -> MirBody
        <$> v .: "vars"
        <*> v .: "blocks"

parsePred :: HML.HashMap Text Value -> Aeson.Parser Predicate
parsePred v = 
  case HML.lookup "kind" v of
    Just (String "Trait") -> do
      TraitPredicate <$> v .: "trait" <*> v .: "substs"
    Just (String "Projection") -> do
      TraitProjection <$> (TyProjection <$> v .: "proj" <*> v .: "substs") <*> v .: "rhs_ty" 
    Just (String "AutoTrait") ->
      AutoTraitPredicate <$> v .: "trait"
    k -> fail $ "cannot parse predicate " ++ show k

instance FromJSON Predicate where
    parseJSON obj = case obj of
      Object v -> do
        mpobj <- v .:? "trait_pred"
        case mpobj of
          Just pobj -> do
            TraitPredicate <$> pobj .: "trait" <*> pobj .: "substs"
          Nothing -> do
            mpobj <- v .:? "trait_proj"
            case mpobj of
              Just ppobj -> do
                pobj <- ppobj .: "projection_ty"
                TraitProjection <$> (TyProjection <$> pobj .: "item_def_id" <*> pobj .: "substs")
                                <*> ppobj .: "ty"
              Nothing -> return UnknownPredicate
      String t | t == "unknown_pred" -> return UnknownPredicate
      wat -> Aeson.typeMismatch "Predicate" wat           

instance FromJSON Param where
    parseJSON = withObject "Param" $ \v ->
      Param <$> v .: "param_def"

instance FromJSON TraitRef where
    parseJSON = withObject "TraitRef" $ \v ->
      TraitRef <$> v .: "trait" <*> v .: "substs"


instance FromJSON TraitImplItem where
    parseJSON = withObject "TraitImplItem" $ \v -> do
      pg <- v .: "generics"
      pp <- v .: "predicates"
      case HML.lookup "kind" v of
        Just (String "Method") -> TraitImplMethod
                                  <$> v .: "name"
                                  <*> v .:? "implements" .!= "unknown[0]::UNKNOWN[0]"
                                  <*> (withObject "Param" (\u -> u .: "params") pg)
                                  <*> (withObject "Predicates" (\u -> u .: "predicates") pp)
                                  <*> v .: "signature"
        Just (String "Type") -> TraitImplType
                                  <$> v .: "name"  
                                  <*> v .:? "implements" .!= "unknown[0]::UNKNOWN[0]"
                                  <*> (withObject "Param" (\u -> u .: "params") pg)
                                  <*> (withObject "Predicates" (\u -> u .: "predicates") pp)
                                  <*> v .: "type"                                  
        Just (String x) -> fail $ "Incorrect format of the kind field in TraitImplItem: " ++ show x
        k -> fail $ "bad kind field in TraitImplItem " ++ show k

instance FromJSON TraitImpl where
  parseJSON = withObject "TraitImpl" $ \v -> do
    pg <- v .: "generics"
    pp <- v .: "predicates"
    tr <- v .: "trait_ref"
    TraitImpl <$> v .: "name"
              <*> pure tr
              <*> pure tr
              <*> withObject "Param" (\u -> u .: "params") pg
              <*> withObject "Predicates" (\u -> u .: "predicates") pp
              <*> v .: "items"
              <*> pure []

instance FromJSON Static where
  parseJSON = withObject "Static" $ \v -> do
    Static <$> v .: "name"
           <*> v .: "ty"
           <*> v .: "mutable"
           <*> v .:? "promoted_from"
           <*> v .:? "promoted_index"

           
--  LocalWords:  initializer supertraits deserialization impls
