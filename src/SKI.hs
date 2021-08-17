-- | This module provides SKI combinator representation of lambda expressions.
--
-- In broad terms, a combinator is a (typically higher-order) function that
-- contains no free variables. More specifically, these functions do not
-- use abstraction and are simply defined as an application of their arguments
-- in some order.
--
-- The combinators used in this module are:
--
-- * @S@, defined as @S x y z = x z (y z)@
-- * @K@, defined as @K x y = x@
-- * @I@, defined as @I x = x@
module SKI
    ( Expr(..)
    , fromNamed
    , toNamed
    , normalForm
    ) where

import Control.DeepSeq
import Data.Set qualified as Set

import Common (s, k, id')
import Expr qualified as N
import Expr (Name)
import Subst

infixl 9 :.

-- | SKI combinator representation of lambda expressions. Free variables are provided for
-- convenience but are not strictly necessary.
--
-- > (λx y. y x) == S :. (K :. (S :. I)) :. K
data Expr
    = FV Name
    | S
    | K
    | I
    | Expr :. Expr

instance NFData Expr where
    rnf (FV x)     = rnf x
    rnf S          = ()
    rnf K          = ()
    rnf I          = ()
    rnf (e1 :. e2) = rnf e1 `seq` rnf e2

instance Show Expr where
    showsPrec = go
      where
        go _ (FV x)     = (x ++)
        go _ S          = ("S" ++)
        go _ K          = ("K" ++)
        go _ I          = ("I" ++)
        go p (e1 :. e2) = showParen (p > 10)
            ( go 10 e1
            . (" " ++)
            . go 11 e2
            )

-- | Transforms a standard (named) representation of a given lambda expression into
-- its SKI combinator representation.
--
-- > ("x" |-> "x") == I
fromNamed :: N.Expr -> Expr
fromNamed = go
  where
    -- Ideally, the helper function would use an intermediate data type that
    -- contains SKI combinators and also abstraction. For simplicity, we reuse
    -- the standard representation and use special variable names for the
    -- combinators.
    go (N.Var "_S") = S
    go (N.Var "_K") = K
    go (N.Var "_I") = I

    go (N.Var x)                   = FV x
    go (e1 N.:. e2)                = go e1 :. go e2
    go (x N.:-> e)
        | x `Set.notMember` free e = K :. go e
    go (_ N.:-> N.Var _)           = I
    go (x N.:-> y N.:-> e)         = go $ x N.:-> relax (go $ y N.:-> e)
    go (x N.:-> e N.:. N.Var _)
        | x `Set.notMember` free e = go e
    go (x N.:-> e1 N.:. e2)        = S :. go (x N.:-> e1) :. go (x N.:-> e2)

    relax (FV x)     = N.Var x
    relax S          = N.Var "_S"
    relax K          = N.Var "_K"
    relax I          = N.Var "_I"
    relax (e1 :. e2) = relax e1 N.:. relax e2

-- | Transforms a SKI combinator representation of a given lambda expression into
-- its standard representation.
--
-- >>> toNamed I
-- λx. x
toNamed :: Expr -> N.Expr
toNamed (FV x)     = N.Var x
toNamed S          = s
toNamed K          = k
toNamed I          = id'
toNamed (e1 :. e2) = toNamed e1 N.:. toNamed e2

-- | Attempts to reduce an expression to a normal form by (lazily) applying
-- the combinator definitions.
--
-- >>> normalForm (S :. S :. K :. I :. K)
-- K I
normalForm :: Expr -> Expr
normalForm (e1 :. e2) = normalForm e1 `app` normalForm e2
  where
    app I             a = a
    app (K :. x)      _ = x
    app (S :. x :. y) a = (x `app` a) `app` (y `app` a)
    app x             a = x :. a
normalForm e = e
