-- | This module provides normal order and applicative order reductions of lambda expressions.
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

-- | Class constraint for computations capable of performing reduction.
type Reduction m = (Alternative m, Fresh m)

-- | Performs a reduction on a given expression. Returns @Nothing@ if no
-- reduction failed.
runReduction :: (forall m. (Reduction m) => Expr -> m Expr) -> Expr -> Maybe Expr
runReduction r e = fst <$> runStateT (r e) (freshest e + 1)

-- | Helper function which passes a reduction to subexpressions of a given
-- expression.
--
-- In case of application, the reduction is first tried on the left subexpression.
pass :: (Reduction m) => (Expr -> m Expr) -> Expr -> m Expr
pass _ (Var _)    = empty
pass r (x :-> e)  = (x :->) <$> r e
pass r (e1 :. e2) = ((:. e2) <$> r e1) <|> ((e1 :.) <$> r e2)

-- | Performs beta reduction. If the given expression is not a beta redex,
-- the reduction fails.
--
-- >>> runReduction beta (("x" |-> "x") :. "y")
-- Just y
--
-- >>> runReduction beta "x"
-- Nothing
beta :: (Reduction m) => Expr -> m Expr
beta ((x :-> e1) :. e2) = subst x e2 e1
beta _                  = empty

-- | Performs one step of the applicative order reduction.
--
-- >>> runReduction appStep (("x y" |-> "x") :. (("z" |-> "z") :. "z"))
-- Just ((λx y. x) z)
appStep :: (Reduction m) => Expr -> m Expr
appStep e = pass appStep e <|> beta e

-- | Performs one step of the normal order reduction.
--
-- >>> runReduction norStep (("x y" |-> "x") :. (("z" |-> "z") :. "z"))
-- Just (λy. (λz. z) z)
norStep :: (Reduction m) => Expr -> m Expr
norStep e = beta e <|> pass norStep e

-- | @steps f m x@ applies the function @f@ to @x@ @m@-times.
steps :: (Monad m) => (a -> m a) -> Int -> a -> m a
steps step n = foldr (>=>) pure $ replicate n step

-- | Attempts to reduce an expression to a normal form using the normal
-- order reduction. Might not terminate.
--
-- >>> normalForm (("x y" |-> "x") :. (("z" |-> "z") :. "z"))
-- λy. z
normalForm :: Expr -> Expr
normalForm expr = case runReduction go expr of
    Just nf -> nf
    _       -> error "normalForm: internal error"
  where
    go e = (norStep e >>= go) <|> pure e
