module SKI
    ( Expr(..)
    , fromNamed
    , toNamed
    , normalForm
    ) where

import Data.Set qualified as Set

import Common (s, k, id')
import Expr qualified as N
import Expr (Name)
import Subst

infixl 9 :.

data Expr
    = FV Name
    | S
    | K
    | I
    | Expr :. Expr
    deriving (Show)

fromNamed :: N.Expr -> Expr
fromNamed = go
  where
    go (N.Var "_S") = S
    go (N.Var "_K") = K
    go (N.Var "_I") = I

    go (N.Var x)                   = FV x
    go (e1 N.:. e2)                = go e1 :. go e2
    go (x N.:-> e)
        | x `Set.notMember` free e = K :. go e
    go (_ N.:-> N.Var _)           = I
    go (x N.:-> y N.:-> e)         = go $ x N.:-> relax (go $ y N.:-> e)
    go (x N.:-> e1 N.:. e2)        = S :. go (x N.:-> e1) :. go (x N.:-> e2)

    relax (FV x)     = N.Var x
    relax S          = N.Var "_S"
    relax K          = N.Var "_K"
    relax I          = N.Var "_I"
    relax (e1 :. e2) = relax e1 N.:. relax e2

toNamed :: Expr -> N.Expr
toNamed (FV x)     = N.Var x
toNamed S          = s
toNamed K          = k
toNamed I          = id'
toNamed (e1 :. e2) = toNamed e1 N.:. toNamed e2

normalForm :: Expr -> Expr
normalForm (e1 :. e2) = normalForm e1 `app` normalForm e2
  where
    app I             a = a
    app (K :. x)      _ = x
    app (S :. x :. y) a = (x `app` a) `app` (y `app` a)
    app x             a = x :. a
normalForm e = e
