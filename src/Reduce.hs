module Reduce
    ( Reduction
    , runReduction
    , appStep
    , norStep
    , steps
    , normalForm
    ) where

import Control.Applicative
import Control.Monad.State

import Expr
import Subst

type Reduction m = (Alternative m, Fresh m)

runReduction :: (forall m. (Reduction m) => m a) -> Maybe a
runReduction r = fst <$> runStateT r 0

pass :: (Reduction m) => (Expr -> m Expr) -> Expr -> m Expr
pass _ (Var _)    = empty
pass r (x :-> e)  = (x :->) <$> r e
pass r (e1 :. e2) = ((:. e2) <$> r e1) <|> ((e1 :.) <$> r e2)

beta :: (Reduction m) => Expr -> m Expr
beta ((x :-> e1) :. e2) = subst x e2 e1
beta _                  = empty

appStep :: (Reduction m) => Expr -> m Expr
appStep e = pass appStep e <|> beta e

norStep :: (Reduction m) => Expr -> m Expr
norStep e = beta e <|> pass norStep e

steps :: (Monad m) => (a -> m a) -> Int -> a -> m a
steps step n = foldr (>=>) pure $ replicate n step

-- | Might not terminate.
normalForm :: Expr -> Expr
normalForm expr = case runReduction (go expr) of
    Just nf -> nf
    _       -> error "normalForm: internal error"
  where
    go e = (norStep e >>= go) <|> pure e
