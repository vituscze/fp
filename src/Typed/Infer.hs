-- | This module provides a definition of the algorithm W, a type inference
-- algorithm for the Hindley-Milner system.
module Typed.Infer
    ( instantiate
    , quantify
    , algorithmW
    , principal
    ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as Map
import Data.Map ((!))
import Data.Set qualified as Set
import Data.Set ((\\))

import Fresh
import Typed.Expr
import Typed.Free
import Typed.Subst
import Typed.Support
import Typed.Unify

-- | Generates a fresh type variable.
freshTy :: (Fresh m) => m Type
freshTy = TyVar <$> fresh

-- | Instantiates a type scheme by replacing all bound variables with
-- fresh free variables.
--
-- >>> evalState (instantiate (Scheme 2 (TyGen 0 :->: TyGen 1))) 1
-- `a → `b
instantiate :: (Fresh m) => Scheme -> m Type
instantiate (Scheme n t) = do
    new <- replicateM n freshTy

    let m = Map.fromList $ zip [0 ..] new

        go (TyVar x)    = TyVar x
        go (TyGen i)    = m ! i
        go (t1 :->: t2) = go t1 :->: go t2

    pure $ go t

-- | Quantifies all free type variables of a type that are not free
-- in the typing context.
--
-- >>> quantify nil ("a" :->: "b")
-- ∀t0 t1. t0 → t1
quantify :: TypeContext -> Type -> Scheme
quantify ctx t = Scheme (Set.size vars) (apply sub t)
  where
    vars = free t \\ free ctx
    sub  = Map.fromList $ zip (Set.toList vars) (map TyGen [0 ..])

-- | The core type inference algorithm. Given a typing context @ctx@ and a lambda
-- term @e@, it produces a pair @(s, t)@ where @s@ is a substitution and
-- @t@ is a type, such that:
--
-- > apply s ctx |- e : t
--
-- That is: term @e@ has a type @t@ in the typing context @apply s ctx@. If no such
-- pair @(s, t)@ exists, the result is a 'TypeError'.
algorithmW :: (Fresh m, MonadError TypeError m)
           => TypeContext
           -> Term
           -> m (Subst, Type)
algorithmW ctx (Var x) = case find x ctx of
    Nothing -> throwError UndefinedVariable
    Just t  -> (,) empty <$> instantiate t
algorithmW ctx (x :-> e) = do
    new <- freshTy
    (s, t) <- algorithmW (extend x (Scheme 0 new) ctx) e
    pure (s, apply s new :->: t)
algorithmW ctx (e1 :. e2) = do
    new <- freshTy
    (s1, t1) <- algorithmW ctx e1
    (s2, t2) <- algorithmW (apply s1 ctx) e2
    r <- unify (apply s2 t1) (t2 :->: new)
    pure (r @@ s2 @@ s1, apply r new)
algorithmW ctx (Let (x, e1) e2) = do
    (s1, t1) <- algorithmW ctx e1
    let ctx' = apply s1 ctx
    (s2, t2) <- algorithmW (extend x (quantify ctx' t1) ctx') e2
    pure (s2 @@ s1, t2)

-- | Finds the principal type of a given term, if it exists.
--
-- >>> principal nil ("x" |-> "x")
-- Right (∀t0. t0 → t0)
--
-- >>> principal nil ("x y z" |-> "x" :. "z" :. ("y" :. "z"))
-- Right (∀t0 t1 t2. (t0 → t2 → t1) → (t0 → t2) → t0 → t1)
--
-- >>> principal nil ("x" |-> "x" :. "x")
-- Left OccursCheck
principal :: TypeContext
          -> Term
          -> Either TypeError Scheme
principal ctx e = (`evalStateT` 1) $ do
    (s, t) <- algorithmW ctx e
    pure $ quantify (apply s ctx) t
