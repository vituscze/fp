-- | This module provdes tools for performing substitutions.
module Subst
    ( rename
    , free
    , subst
    ) where

import Data.Set qualified as Set
import Data.Set (Set)

import Expr
import Fresh

-- | @renameUnsafe src tgt expr@ renames all free occurences of @src@ in the
-- expression @expr@ with the name @tgt@.
--
-- The caller must ensure no free variables are captured.
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

-- | Renames all free occurences of the name with a fresh name. Returns the
-- fresh name and the renamed expression.
rename :: (Fresh m) => Name -> Expr -> m (Name, Expr)
rename src e = do
    tgt <- fresh
    pure (tgt, renameUnsafe src tgt e)

-- | The set of all free variables in a given expression.
--
-- >>> free ("x" :-> "x" :. "y")
-- fromList ["y"]
free :: Expr -> Set Name
free (Var v)    = Set.singleton v
free (x :-> e)  = free e `Set.difference` Set.singleton x
free (e1 :. e2) = free e1 `Set.union` free e2

-- | @subst name what expr@ substitutes the expression @what@ for
-- all free occurrences of the name @name@ in the expression @expr@,
-- automatically renaming bound names in order to avoid free variable
-- capture.
--
-- > evalState (subst "x" ("y" :. "y") ("x" :. "x")) 0 == (("y" :. "y") :. ("y" :. "y"))
subst :: (Fresh m) => Name -> Expr -> Expr -> m Expr
subst x e = go
  where
    fv = free e

    go (Var y)
        | x == y    = pure e
        | otherwise = pure $ Var y
    go (y :-> f)
        | x == y            = pure $ y :-> f
        | y `Set.member` fv = do
            (y', f') <- rename y f
            (y' :->) <$> go f'
        | otherwise         = (y :->) <$> go f
    go (f1 :. f2) = (:.) <$> go f1 <*> go f2
