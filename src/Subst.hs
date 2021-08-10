module Subst
    ( Fresh
    , toFresh
    , fromFresh
    , fresh
    , freshest
    , rename
    , free
    , subst
    ) where

import Control.Monad.State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Set (Set)

import Expr

type Fresh m = (MonadState Int m)

toFresh :: Int -> String
toFresh = ('`' :) . reverse . go
  where
    go n | n <= 0 = ""
         | otherwise = let r = ((n - 1) `mod` 26) + 1
                       in  toEnum (0x60 + r):go ((n - r) `div` 26)

fromFresh :: String -> Int
fromFresh ('`':r) = List.foldl' (\a n -> n + 26 * a) 0 $ map (subtract 0x60 . fromEnum) r
fromFresh _       = 0

fresh :: (Fresh m) => m Name
fresh = state $ \s -> (toFresh s, s + 1)

-- | Find the most recent fresh variable.
freshest :: Expr -> Int
freshest = Maybe.fromMaybe 0 . Set.lookupMax . Set.map fromFresh . go
  where
    go (Var v)    = Set.singleton v
    go (x :-> e)  = go e `Set.union` Set.singleton x
    go (e1 :. e2) = go e1 `Set.union` go e2

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
