{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Wall
                -fno-warn-name-shadowing
                -fno-warn-unused-matches
                -fno-warn-unticked-promoted-constructors #-}

-----------------------------------------------------------------------
-- | Explicitly inherit all supertrait items
--
-- This pass transforms the collection so that
--    1. all traits contain all *items* declared in their supertraits
--    2. all impls are for all subtraits as well as the declared traits
--
-- NOTE: all traits mentioned in supers *must* be declared
-- (i.e. in the traits part of the collection)
--    In other words, this pass must be preceeded by passRemoveUnknownPreds
--    if not, the trait will be eliminated from the collection
-- 
-----------------------------------------------------------------------
module Mir.Pass.ExpandSuperTraits where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Control.Lens((&),(%~),(^.))

import Mir.Mir
import Mir.GenericOps

import GHC.Stack
import Debug.Trace
import Mir.PP

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f (x:xs)
  | Just y <- f x = Just y
  | otherwise     = firstJust f xs
firstJust f []    = Nothing

passExpandSuperTraits :: (?debug::Int, ?mirLib::Collection, HasCallStack) => Collection -> Collection
passExpandSuperTraits col = col & traits   %~ inheritSuperItems
                                & impls    %~ inheritSuperImpls (?mirLib <> col)


inMirLib :: (?debug::Int, ?mirLib::Collection, HasCallStack) => TraitRef -> Bool
inMirLib tr@(TraitRef did ss) =
  Map.member did (?mirLib ^. traits) &&
  any (\ti -> (ti^.tiPreTraitRef) == tr) (?mirLib ^. impls)

inheritSuperImpls :: (?debug::Int, ?mirLib::Collection, HasCallStack) => Collection -> [TraitImpl] -> [TraitImpl]
inheritSuperImpls col tis = Map.elems (go tis Map.empty) where

  -- For a given trait reference, calculate all of its items 
  init :: Map TraitRef TraitImpl
  init = Map.fromList (fmap g (?mirLib ^. impls)) where
     g :: TraitImpl -> (TraitRef, TraitImpl)
     g ti = (ti^.tiPreTraitRef, ti)
  
  go :: HasCallStack => [TraitImpl] -> Map TraitRef TraitImpl -> Map TraitRef TraitImpl
  go trs done = if null this then done else go next step where

     -- divide impls into those we can process now (all supers are done)
     -- and those we need to do later
     (this, next) = List.partition process trs

     -- we can process a traitimpl this step as long as we've already done its supertrait impls
     -- or they are in the standard library
     process :: TraitImpl -> Bool
     process ti = all (\n -> Map.member n done || inMirLib n) (supers (ti^.tiPreTraitRef)) where

     -- find all of the traitrefs for supertraits
     -- this is tricky because we need to get the correct set of type arguments to the trait.
     -- we get these from the predicates associated with the trait that correspond to the
     -- names in the superclass
     supers :: HasCallStack => TraitRef -> [TraitRef]
     supers tr@(TraitRef tn tys)
       | Nothing <- (col^.traits) Map.!? tn = []
            -- ignore supers we can't process error
            -- "BUG: supers: cannot find " ++ fmt tn
     supers tr@(TraitRef tn tys) = supRefs where

        trait     = (col^.traits) Map.! tn
        supNames  =
          (tail (trait^.traitSupers))
        supRefs   = Maybe.mapMaybe isSupPred (trait^.traitPrePreds)

        isSupPred (TraitPredicate did ss) 
          | did `elem` supNames = Just (TraitRef did (tySubst tys ss)) 
        isSupPred _ = Nothing


     addSupers ::  HasCallStack => TraitImpl -> (TraitRef, TraitImpl)
     addSupers ti = (tr, ti & tiItems %~ (++ (concat superItems))
                            & tiPreItems %~ (++ (concat superItems))
                    ) where
       tr         = ti^.tiPreTraitRef
       ss         = supers tr
       superItems = map getItems ss
       getItems tr0 = case (Map.union done init) Map.!? tr0 of
         Just tt -> tt ^.tiPreItems
         Nothing -> (trace $ "Cannot find trait ref " ++ fmt tr0
                    ++ "when adding supers to impl " ++ fmt (ti^.tiTraitRef)
                    ++ "\nsupers are: " ++ fmt ss) []

     step = Map.union done (Map.fromList (map addSupers this))

inheritSuperItems :: (?debug::Int, ?mirLib::Collection, HasCallStack) =>
   Map TraitName Trait -> Map TraitName Trait
inheritSuperItems trs =  Map.map nubTrait (go (Map.elems trs) libTraits) where
  
   -- start with the traits in mirLib as 'done'
   libTraits = (?mirLib) ^. traits
     
   -- remove duplicates
   nubTrait tr = tr & traitItems    %~ List.nub
                    & traitPreItems %~ List.nub

   -- go over all known traits, processing them in topological order
   go :: HasCallStack => [Trait] -> Map TraitName Trait -> Map TraitName Trait
   go trs done = if null this then done else go next step where

      -- divide traits into those we can process now and those for later
      (this, next) = List.partition process trs

      -- we can process a trait as long as we've already done its supers
      process :: Trait -> Bool
      process tr = all (\n -> Map.member n done || n == tr ^.traitName) (tr^.traitSupers)

      addSupers ::  HasCallStack => Trait -> (TraitName, Trait)
      addSupers tr = (tr^.traitName, tr & traitItems %~ (++ newItems)
                                        & traitPreItems %~ (++ newItems)
                     ) where

        
        newItems = concat (map superItems superNames)

        superNames :: [TraitName]
        superNames = filter (/= (tr ^.traitName)) (tr^.traitSupers)

        -- NOTE: we only add items from supertraits that we have a
        -- predicate for in the trait because we need to substitute
        -- 
        -- As a side benefit, I *think* this means that we only add
        -- items from direct super traits, which is actually what we want
        -- (Indirect supertraits will already have their items
        -- incorporated into the direct supertraits).
        -- However, this could cause an issue if a direct supertrait
        -- doesn't include a predicate.
        superItems :: HasCallStack => TraitName -> [TraitItem]
        superItems superName =
          case findSuperPredSubsts superName (tr^.traitPrePreds) of
            Nothing -> []
            Just ss ->
              map (specializeTraitItem ss) rawItems
                where
                  rawItems = (done Map.! superName)^.traitPreItems


        findSuperPredSubsts :: TraitName -> [Predicate] -> Maybe Substs
        findSuperPredSubsts nm [] = Nothing
        findSuperPredSubsts nm (p@(TraitPredicate did ss):_) | nm == did = Just ss
        findSuperPredSubsts nm (_:ps) = findSuperPredSubsts nm ps

        specializeTraitItem :: Substs -> TraitItem -> TraitItem
        specializeTraitItem ss (TraitMethod did sig) = TraitMethod did (tySubst ss sig)
        specializeTraitItem ss (TraitConst did ty)   = TraitConst did (tySubst ss ty)
        specializeTraitItem ss item = item

      step :: Map TraitName Trait
      step = Map.union done (Map.fromList (map addSupers this))
