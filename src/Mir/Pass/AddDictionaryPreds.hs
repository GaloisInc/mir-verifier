{-# LANGUAGE ImplicitParams #-}
module Mir.Pass.AddDictionaryPreds where

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map

import Control.Lens
import GHC.Stack

import Mir.Mir
import Mir.DefId
import Mir.MirTy
import Mir.GenericOps
import Mir.PP

import Debug.Trace

--------------------------------------------------------------------------------------
-- Some functions need additional predicates because they are trait implementations
-- 
-- This pass adds those predicates to trait declarations and then uses those to add them
-- to function implementations
-- 
passAddDictionaryPreds :: (?debug::Int, ?mirLib::Collection, HasCallStack) =>
  Collection -> Collection
passAddDictionaryPreds col = col1 & functions %~ fmap addTraitPreds  where

  col1 = col & traits  %~ fmap addThisPred

  mkPred :: Trait -> Predicate
  mkPred tn = TraitPredicate (tn^.traitName)
                (Substs [TyParam (toInteger i) | i <- [0 .. ((length (tn^.traitParams)) - 1)] ])

  addThisPred :: Trait -> Trait
  addThisPred trait =
         traitWithPreds & traitPreItems .~ (traitWithPreds ^. traitItems)
       where
         traitWithPreds = trait & traitItems %~ map (addThis (mkPred trait))

  -- add predicates to trait methods
  addThis :: Predicate -> TraitItem -> TraitItem
  addThis pred (TraitMethod did sig) = TraitMethod did (sig & fspredicates %~ (addPred [pred]))
  addThis pred ti = ti

  -- add predicates to fn's that are implementation methods
  addTraitPreds :: Fn -> Fn
  addTraitPreds fn = fn & fsig %~ fspredicates %~ (addPred (newPreds fn))

  -- don't add redundant predicates
  addPred :: [Predicate] -> [Predicate] -> [Predicate]
  addPred pred preds = List.nub (pred++preds)

  -- determine the methods that are implementation methods
  -- and the new predicates they should satisfy (== preds for the traits that they impl)
  impls :: Map MethName [Predicate]
  impls = implMethods' col1

  newPreds :: Fn -> [Predicate]
  newPreds fn = Map.findWithDefault [] (fn^.fname) impls 


findMethodItem :: HasCallStack => MethName -> [TraitItem] -> Maybe TraitItem
findMethodItem mn (item@(TraitMethod did fsig):rest) =
  if (mn == did) then Just item else findMethodItem mn rest
findMethodItem mn (_:rest) = findMethodItem mn rest
findMethodItem mn [] = Nothing -- error $ "BUG: cannot find method " ++ fmt mn

implMethods' :: (HasCallStack, ?mirLib::Collection) => Collection -> Map MethName [Predicate]
implMethods' col = foldMap g (concat (Map.elems (col^.impls))) where
  full = ?mirLib <> col
  
  g :: TraitImpl -> Map MethName [Predicate]
  g impl = foldMap g2 (impl^.tiPreItems) where
     TraitRef tn ss = impl^.tiPreTraitRef
     
     items = case (full^.traits) Map.!? tn of
                 Just tr -> tr^.traitPreItems
                 -- Ignore impls that we know nothing about
                 Nothing ->
                   []

     g2 :: TraitImplItem -> Map MethName [Predicate]
     g2 (TraitImplMethod mn ii) =
        case findMethodItem ii items of
          Just (TraitMethod _ sig) ->
            let preds = (tySubst (ss <> (Substs $ TyParam <$> [0 .. ])) (sig^.fspredicates))
            in Map.singleton mn (filter (not . satisfied full) preds)
          _ ->
             Map.empty
             -- ignore unknown methods
             -- error $ "BUG: addDictionaryPreds: Cannot find method " ++ fmt ii ++ " in trait " ++ fmt tn
     g2 _ = Map.empty

defaultMethods :: Collection -> Map MethName TraitName
defaultMethods col = foldr g Map.empty (col^.traits) where
  g trait m = foldr g2 m (trait^.traitPreItems) where
    g2 (TraitMethod methName _sig) m
       | Just _fn <- Map.lookup methName (col^.functions)
       = Map.insert (methName) (trait^.traitName) m
    g2 _ m = m

-- Does this collection trivially satisfy the predicate?
-- always sound to return False
satisfied :: Collection -> Predicate -> Bool
satisfied col (TraitPredicate did ss) = False
satisfied col _ = False
