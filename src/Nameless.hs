-- | This module provides nameless (De Bruijn) representation of lambda expressions.
--
-- Instead of referring to binders by name, we can simply store the distance between
-- a variable and its binder. We define distance as the number of intervening binders.
-- For example, in the expression @λx.λy.x@, the distance between @x@ and its binder
-- is 1 (there is only one intervening binder, @λy.@).
module Nameless
    ( Expr(..)
    , fromNamed
    , toNamed
    , shift
    , subst
    , normalForm
    ) where

import Control.DeepSeq
import Control.Monad.State
import Data.Map qualified as Map
import Data.Map ((!))

import Expr qualified as N
import Expr (Name)
import Subst qualified as S

infixl 9 :.

-- | Nameless representation of lambda expressions. Bound variables are represented by
-- their De Bruijn index (see distance above), free variables by their name (although this
-- is not strictly necessary). Abstraction does not need any information about the bound
-- name.
--
-- > (λx y. y x) == Lam (Lam (BV 0 :. BV 1))
data Expr
    = BV !Int       -- ^ Bound variable
    | FV Name       -- ^ Free variable
    | Lam Expr      -- ^ Abstraction
    | Expr :. Expr  -- ^ Application

instance NFData Expr where
    rnf (BV i)     = rnf i
    rnf (FV x)     = rnf x
    rnf (Lam e)    = rnf e
    rnf (e1 :. e2) = rnf e1 `seq` rnf e2

instance Show Expr where
    showsPrec = go
      where
        go _ (BV i)     = (show i ++)
        go _ (FV x)     = (x ++)
        go p (Lam e)    = showParen (p > 0)
            ( ("λ " ++)
            . go 0 e
            )
        go p (e1 :. e2) = showParen (p > 10)
            ( go 10 e1
            . (" " ++)
            . go 11 e2
            )

-- | Transforms a standard (named) representation of a given lambda expression into
-- its nameless representation.
--
-- > ("x" |-> "x") == Lam (BV 0)
fromNamed :: N.Expr -> Expr
fromNamed = go 0 Map.empty
  where
    go d m (N.Var v)    = case Map.lookup v m of
        Just i  -> BV (d - i)
        Nothing -> FV v
    go d m (x N.:-> e)  = Lam $ go (d + 1) (Map.insert x (d + 1) m) e
    go d m (e1 N.:. e2) = go d m e1 :. go d m e2

-- | Transforms a nameless representation of a given lambda expression into its standard
-- representation. De Bruijn indices must not refer to binders outside of the input
-- expression (such as @Lam (BV 2)@).
--
-- Variable names are picked according to the scheme outlined in "Subst". As a result,
-- free variables of the input expression should not use this naming scheme.
--
-- >>> toNamed (Lam (BV 0))
-- λ`a. `a
toNamed :: Expr -> N.Expr
toNamed = (`evalState` 1) . go 0 Map.empty
  where
    -- Note that we could collect all the free variables of the input
    -- expression and ensure that no free variable conflicts with the
    -- employed fresh variable naming scheme. For simplicity, we skip that
    -- here.
    go d m (BV i) = pure $ N.Var $ m ! (d - i)
    go _ _ (FV x) = pure $ N.Var x
    go d m (Lam e) = do
        v <- S.fresh
        (v N.:->) <$> go (d + 1) (Map.insert (d + 1) v m) e
    go d m (e1 :. e2) = (N.:.) <$> go d m e1 <*> go d m e2

-- | @shift c e@ shifts all variables in the expression @e@ above the cutoff @c@ by one.
--
-- >>> shift 0 (Lam (BV 0 :. BV 1))
-- λ 0 2
shift :: Int -> Expr -> Expr
shift c (BV i)
    | i < c        = BV i
    | otherwise    = BV (i + 1)
shift _ (FV x)     = FV x
shift c (Lam e)    = Lam $ shift (c + 1) e
shift c (e1 :. e2) = shift c e1 :. shift c e2

-- | @subst ix what expr@ substitutes the expression @what@ for
-- all variables with index @ix@ in the expression @expr@.
--
-- >>> subst 1 (Lam (BV 0)) (BV 0 :. BV 1 :. BV 2)
-- 0 (λ 0) 2
subst :: Int -> Expr -> Expr -> Expr
subst n e (BV i)
    | i == n         = e
    | otherwise      = BV i
subst _ _ (FV x)     = FV x
subst n e (Lam f)    = Lam $ subst (n + 1) (shift 0 e) f
subst n e (f1 :. f2) = subst n e f1 :. subst n e f2

-- | A variant of 'subst' that also shifts variables whose index is
-- greater than the variable that is being substituted down by one.
--
-- Used to reduce beta redexes.
bSubst :: Int -> Expr -> Expr -> Expr
bSubst n e (BV i)     = case i `compare` n of
    LT -> BV i
    EQ -> e
    GT -> BV (i - 1)
bSubst _ _ (FV x)     = FV x
bSubst n e (Lam f)    = Lam $ bSubst (n + 1) (shift 0 e) f
bSubst n e (f1 :. f2) = bSubst n e f1 :. bSubst n e f2

-- | Attempts to reduce an expression to a weak head normal form.
--
-- The expression is reduces just enough to ensure there's no beta redex in the
-- head position. No reduction is performed under a binder.
--
-- >>> whnf (Lam (Lam (BV 0) :. BV 0))
-- λ (λ 0) 0
whnf :: Expr -> Expr
whnf (BV i)     = BV i
whnf (FV x)     = FV x
whnf (Lam e)    = Lam e
whnf (e1 :. e2) = case whnf e1 of
    Lam e -> whnf $ bSubst 0 e2 e
    e1'   -> e1' :. e2

-- | Attempts to reduce an expression to a normal form.
--
-- >>> normalForm (Lam (Lam (BV 0) :. BV 0))
-- λ 0
normalForm :: Expr -> Expr
normalForm (BV i)     = BV i
normalForm (FV x)     = FV x
normalForm (Lam e)    = Lam $ normalForm e
normalForm (e1 :. e2) = case whnf e1 of
    Lam e -> normalForm $ bSubst 0 e2 e
    e1'   -> normalForm e1' :. normalForm e2
