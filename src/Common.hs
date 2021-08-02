module Common
    ( id'
    , k
    , k'
    , s
    , y
    ) where

import Expr

-- | > id' = \x. x
id' :: Expr
id' = "x" |-> "x"

-- | > k = \x y. x
k :: Expr
k = "x y" |-> "x"

-- | > k' = \x y. y
k' :: Expr
k' = "x y" |-> "y"

-- | > s = \x y z. x z (y z)
s :: Expr
s = "x y z" |-> "x" :. "z" :. ("y" :. "z")

-- | > y = \f. (\x. f (x x)) (\x. f (x x))
--
-- Does not have a normal form.
y :: Expr
y = "f" |-> ("x" |-> "f" :. ("x" :. "x")) :. ("x" |-> "f" :. ("x" :. "x"))
