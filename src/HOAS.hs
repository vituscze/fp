module HOAS
    ( Expr(..)
    , fromNamed
    , toNamed
    , normalForm
    ) where

import Control.Monad.State
import Data.Map qualified as Map

import Expr qualified as Named
import Expr (Name)

data Expr
    = FV Name
    | Lam (Expr -> Expr)
    | Expr :. Expr

fromNamed :: Named.Expr -> Expr
fromNamed = go Map.empty
  where
    go m (Named.Var x)    = case Map.lookup x m of
        Just x' -> x'
        Nothing -> FV x
    go m (x Named.:-> e)  = Lam (\x' -> go (Map.insert x x' m) e)
    go m (e1 Named.:. e2) = go m e1 :. go m e2

toNamed :: Expr -> Named.Expr
toNamed = (`evalState` names) . go
  where
    names = map ("_" ++) $ concatMap (`replicateM` ['a' .. 'z']) [1 ..]

    go (FV x)     = pure $ Named.Var x
    go (Lam e)    = do
        ~(v:vs) <- get
        put vs
        (v Named.:->) <$> go (e (FV v))
    go (e1 :. e2) = (Named.:.) <$> go e1 <*> go e2

normalForm :: Expr -> Expr
normalForm (FV x)     = FV x
normalForm (Lam e)    = Lam (normalForm . e)
normalForm (e1 :. e2) = case normalForm e1 of
    Lam e -> normalForm $ e e2
    e1'   -> e1' :. normalForm e2
