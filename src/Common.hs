-- | This module provides commonly used lambda expressions.
module Common
    ( id'
    , k
    , k'
    , s
    , y
    ) where

import Expr

-- | > id' = λx. x
id' :: Expr
id' = "x" |-> "x"

-- | > k = λx y. x
k :: Expr
k = "x y" |-> "x"

-- | > k' = λx y. y
k' :: Expr
k' = "x y" |-> "y"

-- | > s = λx y z. x z (y z)
s :: Expr
s = "x y z" |-> "x" :. "z" :. ("y" :. "z")

-- | The fixed-point combinator. Does not have a normal form.
--
-- > y = λf. (λx. f (x x)) (λx. f (x x))
y :: Expr
y = "f" |-> ("x" |-> "f" :. ("x" :. "x")) :. ("x" |-> "f" :. ("x" :. "x"))
