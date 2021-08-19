module Typed.Infer
    ( runInfer
    , instantiate
    , quantify
    , algorithmW
    ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as Map
import Data.Map ((!))
import Data.Set qualified as Set
import Data.Set ((\\))

import Subst (Fresh, fresh)
import Typed.Expr
import Typed.Free
import Typed.Subst
import Typed.Support
import Typed.Unify

runInfer :: (forall m. (Fresh m, MonadError TypeError m) => m a) -> Either TypeError a
runInfer = (`evalStateT` 1)

freshTy :: (Fresh m) => m Type
freshTy = TyVar <$> fresh

instantiate :: (Fresh m) => Scheme -> m Type
instantiate (Scheme n t) = do
    new <- replicateM n freshTy

    let m = Map.fromList $ zip [0 ..] new

        go (TyVar x)    = TyVar x
        go (TyGen i)    = m ! i
        go (t1 :->: t2) = go t1 :->: go t2

    pure $ go t

quantify :: TypeContext -> Type -> Scheme
quantify ctx t = Scheme (Set.size vars) (apply sub t)
  where
    vars = free t \\ free ctx
    sub  = Map.fromList $ zip (Set.toList vars) (map TyGen [0 ..])

algorithmW :: (Fresh m, MonadError TypeError m)
           => TypeContext
           -> Expr
           -> m (Subst, Type)
algorithmW ctx (Var x) = case find x ctx of
    Nothing    -> throwError UnknownVariable
    Just genTy -> (,) empty <$> instantiate genTy
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
