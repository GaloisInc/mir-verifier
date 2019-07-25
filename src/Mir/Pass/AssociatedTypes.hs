-- Pass to remove associated types from the collection
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Mir.Pass.AssociatedTypes(passAssociatedTypes,addLocalATs) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Control.Monad.Except(MonadError(..),when)

import Control.Lens((^.),(%~),(&),(.~))

import Mir.DefId
import Mir.Mir
import Mir.MirTy
import Mir.GenericOps
import Mir.PP
import Text.PrettyPrint.ANSI.Leijen(Pretty(..))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Debug.Trace
import GHC.Stack

--
-- Debugging aid
--
traceMap :: (Pretty k, Pretty v) => Map.Map k v -> a -> a
traceMap ad x =
   let doc = Map.foldrWithKey (\k v a -> (pretty k PP.<+> pretty v) PP.<$> a) PP.empty ad
   in trace (fmt doc) $ x


mkDictWith :: (Foldable t, Ord k) => (a -> Map k v) -> t a -> Map k v
mkDictWith f = foldr (\t m -> f t `Map.union` m) Map.empty

traceCollection_AT :: Collection -> a -> a
traceCollection_AT col = seq col .
     trace ("v--------------------------------------v") .
     trace ("Functions with ATs: \n" ++ fmt functionsWithATs) .
     trace ("Impls with ATs: \n" ++ fmt implsWithATs)         .
     trace ("Traits with ATs: \n" ++ fmt traitsWithATs)       .
     trace ("^--------------------------------------^")     
  where         
       functionsWithATs = filter (not . null . (^. fsig . fsassoc_tys)) (Map.elems (col ^. functions))
       implsWithATs     = filter (not . null . (^. tiAssocTys)) (concat (Map.elems (col ^. impls)))
       traitsWithATs    = filter (not . null . (^. traitAssocTys)) (Map.elems (col ^. traits))



--------------------------------------------------------------------------------------
-- This pass turns "associated types" into additional type arguments to polymorphic
-- functions
--
-- An associated type is defined as
-- @
-- type AssocTy = (DefId, Substs)
-- @
--   and is isomorphic to a `Ty` of the form `(TyProjection DefId Substs)`
--
-- we record the associations via the ATDict data structure
-- @
-- type ATDict  = Map DefId (Substs -> Maybe Ty)
-- @
-- and use this data structure to translate the collection to eliminate all uses
-- of TyProjection from the MIR AST.  
--
--
-- This translation happens in stages
--
-- 1. *identify* the associated types in all traits, and record them
--     (addTraitAssocTys)
--
-- 2. update the adict from the impls and add "custom" associations
--      because ATs can be defined in terms of other ATs, we need to process the 
--      impls topologically to make sure that the result is AT-free.
--
-- 3. identify associated types in Fns traits and impls, *ignoring* those that can be
--    satisfied by the global adict (addFnAssocTys)
--
-- 4. create a data structure for easily finding the trait that a method belongs to
--    (buildMethodContext).
--
-- 5. translate the entire collection to eliminate uses of `TyProjection`
--    and add extra type arguments to methods 
--
-- (NOTE: some of the implementation of this pass is "abstractATs" in Mir.GenericOps)
--
passAssociatedTypes :: (?debug::Int, ?mirLib::Collection, HasCallStack) => Collection -> Collection
passAssociatedTypes col =
   let -- make mapping from ATs to their definitions, based on impls
       -- as well as some custom ATs (closureATDict, indexATDict)
       adict0 = implATDict ?mirLib <> closureATDict ?mirLib <> indexATDict
       -- update mirLib to include the correct ADict (? maybe this doesn't need to be done?)
       col0   = ?mirLib & adict .~ adict0 in
   let ?mirLib = col0 in
     
   let -- 1. identify traits that have AssociatedTypes in the collection to be processed.
       -- (this information is recorded in the trait)
       -- update the traits in the collection to include the ATs
       onlyTraitsWithATs   =
         Map.filter (not . null . (^.traitAssocTys)) (calcTraitAssocTys (col ^. traits))
         
       col1  =
         col  & traits    %~ Map.union onlyTraitsWithATs

       -- 2. create dictionary of known associated types
       -- and add it to the current collection
       col2   = col1 & adict .~ (implATDict col1     <> closureATDict col1)

       -- 3. find ATs for functions, fn types in traitItems and impls
       -- We (ab)use the ATInfo to prune out statically known ATs
       col3  =
         col2 & functions %~ fmap (addFnAssocTys info1)
              & traits    %~ fmap (\tr -> tr & traitItems %~ fmap (addTraitFnAssocTys info1 tr))
              & impls     %~ fmap (addImplAssocTys info1)
           where info1 =  ATInfo 0 0 (?mirLib <> col2)
                            (error "Should ONLY need collection here")
                          
       full3 = ?mirLib <> col3

       -- 4. Invert the "impls" (i.e. mapping from method name -> implementing trait)
       mc    = buildMethodContext full3

       -- 5. translate everything
       col4  = col3 & traits    %~ Map.map (translateTrait info) 
                    & functions %~ Map.map (translateFn    info)
                    & impls     %~ Map.map (map (translateImpl  info))
          where info  = ATInfo 0 0 full3 mc
      
   in
     --traceCollection_AT col4  $
     col4


----------------------------------------------------------------------------------------
-- | Calculate associated types from predicates
--
-- We need to know which *traits* already have ATs in order to call this function
--
-- NOTE: don't add associated types (i.e. new params) for ATs that we
-- already have definitions for in adict and make sure to remove duplicates
--

-- Traits that we should never add associated types for
noAssoc :: [DefId]
noAssoc = [textId "::ops[0]::function[0]::FnOnce[0]",
           textId "::ops[0]::function[0]::Fn[0]",
           textId "::ops[0]::function[0]::FnMut[0]" ]


-- | Extract untranslated ATs from the predicates
-- (These include ATs from both regular predicates and TraitProjections)
predATs :: HasCallStack => Map TraitName Trait -> Predicate -> [AssocTy]
predATs d (TraitPredicate did ss)
  | did `elem` noAssoc
  = []
  | Just tr <- Map.lookup did d
  = 
    tySubst ss (tr^.traitAssocTys)
  | otherwise
  = 
  []
predATs _d (TraitProjection lhs rhs) =
  filter (\(did,ss) -> not (getTraitName did `elem` noAssoc)) (tyProjections lhs ++ tyProjections rhs)
predATs _d UnknownPredicate = []          


-- Calculate for ATs traits (don't try to prune)
calcPredAssocTys ::  HasCallStack => Map TraitName Trait -> [Predicate] -> [AssocTy]
calcPredAssocTys d preds =
  (List.nub (concat (map (predATs d) preds)))


-- Calculate for everything else (try to prune if already satisfied)
atiPredAssocTys ::  HasCallStack => ATInfo -> [Predicate] -> [AssocTy]
atiPredAssocTys ati preds = atys where    
    raw_atys = concat (map (predATs (ati^.atCol.traits)) preds)
    tr_atys  = case abstractATs ati (map (\(s,dd)-> TyProjection s dd) raw_atys) of
                   Right ats -> (map (\x -> case x of
                                          TyProjection dd ss -> Just (dd,ss)
                                          _ -> Nothing) ats)
                   Left _ -> map Just raw_atys
    atys = List.nub (Maybe.catMaybes tr_atys)


----------------------------------------------------------------------------------------
-- ** 1. Calculating associated types for traits and add this info to the AST


-- Trait ATs come from two sources:
--   1. The type items (where the arguments to the ATs are the original params of the trait)
--   2. Predicates for this trait that mention other traits with ATs
--
-- NOTE: because of (2) we have to process traits in dependency order, calculating their ATs
-- only after all of the traits that they mention in their predicates have been processed.
calcTraitAssocTys :: HasCallStack => Map TraitName Trait -> Map TraitName Trait
calcTraitAssocTys trs = go (Map.elems trs) Map.empty where
  go :: HasCallStack => [Trait] -> Map TraitName Trait -> Map TraitName Trait
  
  addTraitATs :: HasCallStack => Trait -> Map TraitName Trait -> Maybe (Map TraitName Trait)
  addTraitATs tr done = if all (`Map.member` done) refs then
                          Just (Map.insert (tr^.traitName) trait' done)
                        else
                          Nothing
      where
        -- trait names mentioned in this trait's predicates (except for this one)
        refs    = filter (not . (== tr^.traitName)) (concat (map predRef (tr^.traitPredicates)))
        -- trait updated with AT 
        trait'  = tr & traitAssocTys .~  List.nub (atItems ++ atRefs)
        
        -- 1. ATs from type items in this trait
        atItems = map (,subst) [ did | (TraitType did) <- tr^.traitItems,
                                 did /= textId "::ops[0]::function[0]::FnOnce[0]::Output[0]" ]
        subst   = Substs [ TyParam (toInteger i)
                          | i <- [0 .. (length (tr^.traitParams) - 1)] ]
                  
        -- 2. ATs from predicate mentions
        atRefs  = calcPredAssocTys done (tr^.traitPredicates)


  go trs done =
    if null next
       then step
       else if length next == length trs then
              error $ "BUG in calcTraitAssocTys: not making progress on " ++ fmt trs
              else go next step where
      (step, next) = foldMaybe addTraitATs trs done

-- All traits that are referred to in a predicate
predRef :: Predicate -> [TraitName]
predRef (TraitPredicate did _ss)  = [did]
predRef (TraitProjection lhs rhs) = (map projectionTrait (tyProjections lhs)
                                     ++ map projectionTrait (tyProjections rhs))
  where projectionTrait :: (DefId, Substs) -> TraitName
        projectionTrait (did, _ss) =
          getTraitName did
predRef UnknownPredicate = []          



-- Impl ATs come from predicates of this trait that mention other traits with ATs
addImplAssocTys :: (HasCallStack) => ATInfo -> [TraitImpl] -> [TraitImpl]
addImplAssocTys ati impls = map addImplATs impls where
  addImplATs :: TraitImpl -> TraitImpl
  addImplATs ti = ti & tiAssocTys .~ atiPredAssocTys ati (ti^.tiPredicates) where

-- | Update a fnSig with ATs for the function
addFnSigAssocTys :: HasCallStack => ATInfo -> FnSig -> FnSig
addFnSigAssocTys ati sig =
--  (if (not (null (newsig ^.fsassoc_tys ))) then trace $ "Adding ATs to " ++ fmt newsig
--   else (trace ("no ATs for " ++ fmt sig )))
  newsig where
     newsig = sig & fsassoc_tys .~ atiPredAssocTys ati (sig^.fspredicates)

-- | Update the function with information about associated types
addFnAssocTys :: HasCallStack => ATInfo -> Fn -> Fn
addFnAssocTys ati fn =
  fn & fsig %~ addFnSigAssocTys ati 
  
addTraitFnAssocTys :: HasCallStack => ATInfo -> Trait -> TraitItem -> TraitItem
addTraitFnAssocTys ati tr (TraitMethod did sig) =
     TraitMethod did (addFnSigAssocTys ati' sig)
  where
    -- extend the dictionary with associated type definitions from the trait
    adict' = addLocalATs (toInteger (length (tr^.traitParams))) (tr^.traitAssocTys)
    ati'   = ati & atCol %~ adict'
addTraitFnAssocTys ati tr it = it

----------------------------------------------------------------------------------------
foldMaybe :: (a -> b -> Maybe b) -> [a] -> b -> (b, [a])
foldMaybe f [] b = (b,[])
foldMaybe f (x:xs) b =
  let (b',as) = foldMaybe f xs b in
  case f x b' of
    Just b'' -> (b'',as)
    Nothing -> (b',x:as)
  
      
-- | Create a mapping from associated types (DefId,Substs) to their definitions
--   based on impls.
--
-- NOTE: because ATs can be defined in terms of other TyProjections, we need to
-- create this dictionary incrementally, only adding the ATs from an impl if
-- we can already translate all of the TyProjections in its RHS
--
-- Furthermore, to do this incrementally, we need to compute this information
-- only from stashed away *pre-translated* info about the traits and impls
-- What this means is
--    1. we look up the TraitRef from an impl using the tiPreTraitRef
--    2. 
implATDict :: (?mirLib :: Collection, HasCallStack, ?debug::Int) => Collection -> ATDict
implATDict col = go (concat (Map.elems (col^.impls))) mempty where
  full = ?mirLib <> col

  -- Try to process a TraitImpl, returning whether the processing was successful
  -- or whether we need to wait for other traitImpls to be processed first.
  addImpl :: TraitImpl -> ATDict -> Maybe ATDict
  addImpl ti done = case result of
       Just m -> do
         Just m
       Nothing -> Nothing
    where 
    result = foldr addImplItem (Just done) (ti^.tiItems)
    TraitRef tn ss = ti^.tiPreTraitRef

    -- ATInfo for translating the RHS of TraitImplTypes
    -- This ATInfo needs to include ATs from the impl itself
    atinfo = ATInfo start num (add full) (error "Only type components")
    add    = addLocalATs start atys        
    start  = toInteger (length (ti^.tiGenerics))
    num    = toInteger (length (ti^.tiAssocTys))
    atys   = ti^.tiAssocTys
    
    addImplItem :: TraitImplItem -> Maybe ATDict -> Maybe ATDict
    addImplItem tii@(TraitImplType _ ii ty) (Just m) = do
      -- try to translate the RHS
      ty' <- case abstractATs atinfo ty of
                   Left s ->  Nothing
                   Right v -> Just v
      when (?debug > 3) $ do
         traceM $ "adding " ++ fmt ii ++ fmt ss ++ " |-> " ++ fmt ty'
         --traceM $ "traitRef is " ++ fmt (ti^.tiPreTraitRef)
         --traceM $ "atys is " ++ fmt atys
      Just $ insertATDict (ii,ss) ty' m where
    addImplItem _ Nothing = Nothing
    addImplItem _ m = m
  
  go :: [TraitImpl] -> ATDict -> ATDict  
  go tis done =
    if null next
       then this
       else if length next == length tis then
              if (?debug > 1) then 
                  (trace $ "BUG in mkImplADict: not making progress during implATDict." ++
                  "\nDropping remaining impls. " ++ fmt (map (^.tiTraitRef) tis)) this
              else
               this
            else go next step where

    (this, next) = foldMaybe addImpl tis done

    step = this
  

-- Add entries to ATDict for the "FnOnce::Output" associated type
-- For various *concrete* function types
closureATDict :: HasCallStack => Collection -> ATDict
closureATDict col =
  singletonATDict (textId "::ops[0]::function[0]::FnOnce[0]::Output[0]")
     (\ substs -> case substs of
         Substs [TyClosure fname _ss, cty] ->
           case (col^.functions) Map.!? fname of
             Nothing -> Nothing
             Just fn -> Just (fn^.fsig^.fsreturn_ty)
         Substs [TyFnPtr sig] ->
             Just (sig^.fsreturn_ty)
         Substs [TyFnDef fname args,_] ->
           case (col^.functions) Map.!? fname of
             Nothing -> Nothing
             Just fn -> Just (fn^.fsig^.fsreturn_ty)
         Substs [TyDynamic _, TyTuple [ret]] ->
           Just ret
         _ -> Nothing)

-- Working around limitations in translating the stdlib
--
-- Custom ATs for:
--   type Index::Output<[T],I> == SliceIndex<I,[T]>
--   type ::iter::iterator::Iterator::Item<::slice::IterMut<lifetime,T>> == &mut T
indexATDict :: HasCallStack => ATDict
indexATDict = mempty
{-  
  (mconcat
   [singletonATDict (textId "::ops[0]::index[0]::Index[0]::Output[0]")
    (\ substs -> case substs of
        Substs [TySlice elt, ii]
          -> Just (TyProjection (textId "::slice[0]::SliceIndex[0]::Output[0]") (Substs [ii, TySlice elt]))
        Substs _ ->
          Nothing)
    
  , singletonATDict (textId "::iter[0]::iterator[0]::Iterator[0]::Item[0]")
    (\ substs ->
       case substs of 
        Substs [TyAdt did (Substs [lifetime,param])]
          | did == textId "::slice[0]::::IterMut[0]"
          -> Just (TyRef param Mut)
          
        Substs _ ->
          Nothing)
  ]) -}

   
----------------------------------------------------------------------------------------

-- | Pre-allocate the trait info so that we can find it more easily
-- This map translates trait methods to their analogues in the translated versions
-- NOTE: should we pull out the sig in the translated trait???
buildMethodContext :: HasCallStack => Collection -> Map MethName (MethName, FnSig, Trait)
buildMethodContext col = foldMap go (col^.traits) where
   go tr =
    case Map.lookup (tr^.traitName) (col^.traits) of
      Just tr2 ->
         foldMap go2 (tr^.traitItems) where
             go2 (TraitMethod nm sig) = Map.singleton nm (nm, sig, tr2)
             go2 _ = Map.empty
      Nothing -> Map.empty
     
-----------------------------------------------------------------------------------
-- ** Actual translation for traits, impls and Functions

-- | In this part, we need to be able to translate everything. It's a bug if we don't
-- have a definition for a TyProjection here.
abstractATsE :: (GenericOps a, Pretty a, HasCallStack, ?debug::Int) => ATInfo -> a -> a
abstractATsE ati x = case abstractATs ati x of
                       Left s  -> (if ?debug > 1 then
                                    trace ("BUG:** " ++ s ++ "\n**when abstractATs on " ++ fmt x)
                                  else
                                    id) x
                       Right v -> v


-- | trait declarations 
-- add associated types to the end of the current params, translate items and predicates
translateTrait :: (HasCallStack,?debug::Int) => ATInfo -> Trait -> Trait
translateTrait ati trait =
  --trace ("Translating " ++ fmt (trait ^.traitName)
  --       ++ "\n assocTys: " ++ fmt (trait ^.traitAssocTys))
  trait1
     where
       trait1 = trait & traitItems      %~ map updateMethod
                      & traitPredicates %~ map (abstractATsE info)
                      & traitParams     %~ (++ (map toParam) atys)
                      

       info   = ati & atStart .~ j
                    & atNum   .~ toInteger (length atys)
                    & atCol   %~ addLocalATs j atys
                    
       atys = trait^.traitAssocTys
       j = toInteger $ length (trait^.traitParams)
       
       -- Translate types of methods and add new type parameters for the trait's ATs.
       -- Todo: remove type items?
       updateMethod (TraitMethod name sig) =
             let sig' = abstractATsE info sig
                       & fsgenerics %~ insertAt (map toParam atys) (fromInteger j)
             in 
             TraitMethod name sig'
       updateMethod item = item

-- | Update trait implementations with additional generic types instead of
-- associated types

-- This involves:
--    adding impl ATs to the generics
--    updating the local adict with new ATs, as well as any ATs defined in this impl
translateImpl :: (HasCallStack,?debug::Int) => ATInfo -> TraitImpl -> TraitImpl
translateImpl ati impl = newImpl
     where
       newImpl = impl & tiTraitRef    .~ newTraitRef
                      & tiPreTraitRef .~ (TraitRef tn ss)
                      & tiGenerics    %~ (++ (map toParam atys))
                      & tiPredicates  %~ map (abstractATsE info)
                      & tiItems       %~ Maybe.mapMaybe translateImplItem
                      
       TraitRef tn ss = impl^.tiTraitRef                             
       newTraitRef = TraitRef tn (ss <> ss')
       
       info   = ati & atStart .~ j
                    & atNum   .~ toInteger (length atys)
                    & atCol   %~ addLocalATs j atys
       col    = ati^.atCol
       atys   = impl^.tiAssocTys
       j      = toInteger $ length (impl^.tiGenerics)       


       -- Update the TraitRef to include ATs
       -- If we don't have info about the trait, assume it has no ATs
       trATs = case (col^.traits) Map.!? tn of
                       Just trait -> tySubst ss (trait^.traitAssocTys)
                       Nothing    -> []
                       
       ss'  = case lookupATs info trATs of
                Left s -> error $ s ++ "\n when looking up atys " ++ fmt trATs
                Right v -> v
       
       translateImplItem ti@(TraitImplMethod nm did) = Just $ (TraitImplMethod nm did)
{-         ti & tiiGenerics   .~ newsig ^. fsgenerics
            & tiiPredicates .~ newsig ^. fspredicates
            & tiiSignature  .~ newsig
           where
             newsig :: Fang
             newsig = abstractATsE info sig -}
       -- TODO: remove?                                
       translateImplItem ti@(TraitImplType {})  = Nothing

       



-- Fn translation for associated types
--
-- 1. find <atys>, which were previously calculated
-- 2. these atys will be new params at the end of the fn params
-- 3. update <info> by extending ATdict with new ATs & recording location of new ATs
-- 4. translate all other components of the Fn 

-- update preds if they mention traits with associated types
-- update args and retty from the types to refer to trait params instead of assoc types
-- add assocTys if we abstract a type bounded by a trait w/ an associated type
translateFn :: (HasCallStack, ?debug::Int) => ATInfo -> Fn -> Fn
translateFn ati fn = newFn
  where 
     newFn = fn & fargs       %~ fmap (\v -> v & varty %~ abstractATsE info)
                & fsig        .~ newSig
                & fbody       %~ abstractATsE info

     newSig = fn ^.fsig & fsarg_tys    %~ map (abstractATsE info)
                        & fsreturn_ty  %~ abstractATsE info
                        & fspredicates %~ map (abstractATsE info)
                        & fsgenerics   %~ (++ (map toParam atys))                                 
              
     atys = fn^.fsig.fsassoc_tys
     -- use the old sig to figure out where to add the new params
     j = toInteger $ length (fn^.fsig.fsgenerics)
     
     info   = ati & atStart .~ j
                  & atNum   .~ toInteger (length atys)
                  & atCol  %~ addLocalATs j atys







