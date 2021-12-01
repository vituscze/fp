-- | This module provides type-level substitutions.
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

-- | A substitution is a mapping from names to types.
type Subst = Map Name Type

-- | The empty substitution
empty :: Subst
empty = Map.empty

-- | Creates a substitution from a single variable-type pairing.
--
-- >>> singleton "x" ("y" :->: "y")
-- fromList [("x",y → y)]
singleton :: Name -> Type -> Subst
singleton = Map.singleton

-- | Composes two substitutions. Note that this operation is not
-- commutative: there exist @x@ and @y@ such that @x \@\@ y /= y \@\@ x@.
-- The resulting substitution satisfies:
--
-- > apply (s1 @@ s2) t == apply s1 (apply s2 t)
--
-- >>> singleton "x" ("y" :->: "y") @@ singleton "y" ("z" :->: "z")
-- fromList [("x",y → y),("y",z → z)]
--
-- >>> singleton "y" ("z" :->: "z") @@ singleton "x" ("y" :->: "y")
-- fromList [("x",(z → z) → z → z),("y",z → z)]
(@@) :: Subst -> Subst -> Subst
s @@ t = (apply s `Map.map` t) `Map.union` s

-- | Minimal complete definition: 'apply'
class Apply t where
    -- | Apply a substitution to a given expression.
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
