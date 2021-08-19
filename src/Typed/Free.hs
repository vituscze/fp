module Typed.Free
    ( Free(..)
    ) where

import Data.Set qualified as Set
import Data.Set (Set)

import Typed.Expr
import Typed.Support

class Free t where
    free :: t -> Set Name

instance Free Type where
    free (TyVar x)    = Set.singleton x
    free (TyGen _)    = Set.empty
    free (t1 :->: t2) = free t1 `Set.union` free t2

instance Free Scheme where
    free (Scheme _ t) = free t

instance Free TypeContext where
    free (TyCtx m) = foldr (Set.union . free) Set.empty m
