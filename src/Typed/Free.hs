-- | This module provides tools to collect free (type) variables of types, type schemes or type contexts.
module Typed.Free
    ( Free(..)
    ) where

import Data.Set qualified as Set
import Data.Set (Set)

import Typed.Expr
import Typed.Support

-- | Minimal complete definition: 'free'
class Free t where
    -- | Extracts the set of free variables from a given expression.
    free :: t -> Set Name

instance Free Type where
    free (TyVar x)    = Set.singleton x
    free (TyGen _)    = Set.empty
    free (t1 :->: t2) = free t1 `Set.union` free t2

instance Free Scheme where
    free (Scheme _ t) = free t

instance Free TypeContext where
    free (TyCtx m) = foldr (Set.union . free) Set.empty m
