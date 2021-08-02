module Subst
    ( Fresh
    , rename
    , free
    , subst
    ) where

import Control.Monad.State
import Data.Set qualified as Set
import Data.Set (Set)

import Expr

type Fresh m = (MonadState Int m)

fresh :: (Fresh m) => m Name
fresh = state $ \s -> ("f" ++ show s, s + 1)

-- | The caller must ensure no free variables are captured.
renameUnsafe :: Name -> Name -> Expr -> Expr
renameUnsafe src tgt = go
  where
    go (Var x)
        | x == src  = Var tgt
        | otherwise = Var x
    go (x :-> e)
        | x == src  = x :-> e
        | otherwise = x :-> go e
    go (e1 :. e2)   = go e1 :. go e2

rename :: (Fresh m) => Name -> Expr -> m (Name, Expr)
rename src e = do
    tgt <- fresh
    pure (tgt, renameUnsafe src tgt e)

free :: Expr -> Set Name
free (Var v)    = Set.singleton v
free (x :-> e)  = free e `Set.difference` Set.singleton x
free (e1 :. e2) = free e1 `Set.union` free e2

subst :: (Fresh m) => Name -> Expr -> Expr -> m Expr
subst x e (Var y)
    | x == y    = pure e
    | otherwise = pure $ Var y
subst x e (y :-> f)
    | x == y                = pure $ y :-> f
    | y `Set.member` free e = do
        (y', f') <- rename y f
        (y' :->) <$> subst x e f'
    | otherwise             = (y :->) <$> subst x e f
subst x e (f1 :. f2) = (:.) <$> subst x e f1 <*> subst x e f2
