-- | This module provides HOAS (higher-order abstract syntax) representation of lambda expressions.
--
-- The HOAS representation does not define its own variable binding. Instead, it uses the variable
-- binding mechanisms of the source language. The main advantage is that substitution does not need
-- to be implemented (it is provided by the source language). This benefit comes at the cost of
-- decreased control over evaluation (again, provided by the source language to some degree).
module HOAS
    ( Expr(..)
    , fromNamed
    , toNamed
    , normalForm
    ) where

import Control.Monad.State
import Data.Map qualified as Map

import Expr qualified as N
import Expr (Name)
import Subst qualified as S

infixl 9 :.

-- | HOAS representation of lambda expressions. Bound variables are represented as variables
-- of the host language, lambda abstractions as functions. Free variables are represented
-- explicitly for convenience.
--
-- > (λx y. y x) == Lam (\x -> Lam (\y -> y :. x))
data Expr
    = FV Name             -- ^ Free variable
    | Lam (Expr -> Expr)  -- ^ Abstraction
    | Expr :. Expr        -- ^ Application

-- No NFData instance since we cannot reduce under the binder.

instance Show Expr where
    showsPrec p = showsPrec p . toNamed

-- | Transforms a standard (named) representation of a given lambda expression into
-- its HOAS representation.
--
-- > ("x" |-> "x") == Lam (\x -> x)
fromNamed :: N.Expr -> Expr
fromNamed = go Map.empty
  where
    go m (N.Var x)    = case Map.lookup x m of
        Just x' -> x'
        Nothing -> FV x
    go m (x N.:-> e)  = Lam (\x' -> go (Map.insert x x' m) e)
    go m (e1 N.:. e2) = go m e1 :. go m e2

-- | Transforms a HOAS representation of a given lambda expression into its standard
-- representation.
--
-- Variable names are picked according to the scheme outlined in "Subst". As a result,
-- free variables of the input expression should not use this naming scheme.
--
-- >>> toNamed (Lam (\x -> x))
-- λ`a. `a
toNamed :: Expr -> N.Expr
toNamed = (`evalState` 1) . go
  where
    -- Note that we could collect all the free variables of the input
    -- expression and ensure that no free variable conflicts with the
    -- employed fresh variable naming scheme. For simplicity, we skip that
    -- here.
    go (FV x)     = pure $ N.Var x
    go (Lam e)    = do
        v <- S.fresh
        (v N.:->) <$> go (e (FV v))
    go (e1 :. e2) = (N.:.) <$> go e1 <*> go e2

-- | Attempts to reduce an expression to a weak head normal form.
--
-- The expression is reduces just enough to ensure there's no beta redex in the
-- head position. No reduction is performed under a binder.
--
-- >>> whnf (Lam (\x -> Lam (\y -> y) :. x))
-- λ`a. (λ`b. `b) `a
whnf :: Expr -> Expr
whnf (FV x)     = FV x
whnf (Lam e)    = Lam e
whnf (e1 :. e2) = case whnf e1 of
    Lam e -> whnf $ e e2
    e1'   -> e1' :. e2

-- | Attempts to reduce an expression to a normal form.
--
-- >>> normalForm (Lam (\x -> Lam (\y -> y) :. x))
-- λ`a. `a
normalForm :: Expr -> Expr
normalForm (FV x)     = FV x
normalForm (Lam e)    = Lam (normalForm . e)
normalForm (e1 :. e2) = case whnf e1 of
    Lam e -> normalForm $ e e2
    e1'   -> normalForm e1' :. normalForm e2
