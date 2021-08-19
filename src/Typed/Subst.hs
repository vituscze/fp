module Typed.Subst
    ( Subst
    , empty
    , singleton
    , (@@)
    , Apply(..)
    ) where

import Data.Map (Map)
import Data.Map qualified as Map

import Typed.Expr
import Typed.Support

infixr 4 @@

type Subst = Map Name Type

empty :: Subst
empty = Map.empty

singleton :: Name -> Type -> Subst
singleton = Map.singleton

(@@) :: Subst -> Subst -> Subst
s @@ t = Map.foldrWithKey (\u -> Map.insert u . apply s) s t

class Apply t where
    apply :: Subst -> t -> t

instance Apply Type where
    apply s (TyVar v)    = case Map.lookup v s of
        Nothing -> TyVar v
        Just t  -> t
    apply _ (TyGen i)    = TyGen i
    apply s (t1 :->: t2) = apply s t1 :->: apply s t2

instance Apply Scheme where
    apply s (Scheme i t) = Scheme i (apply s t)

instance Apply TypeContext where
    apply s (TyCtx m) = TyCtx (Map.map (apply s) m)
