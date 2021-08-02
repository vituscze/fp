module Scott
    (
    -- * Natural numbers
      nat
    , zero
    , suc
    , add
    , mul
    , pred'
    , toInt

    -- * Lists
    , nil
    , cons
    , list
    , toList
    ) where

import Common
import Expr
import Reduce

nat :: Int -> Expr
nat 0 = zero
nat n = suc :. nat (n - 1)

-- | > zero = \s z. z
zero :: Expr
zero = "s z" |-> "z"

-- | > suc = \a s z. s a
suc :: Expr
suc = "a s z" |-> "s" :. "a"

-- | > add = y \r a b. a (\pa. suc (r pa b)) b
add :: Expr
add = y :. ("r a b" |-> "a" :. ("pa" |-> suc :. ("r" :. "pa" :. "b")) :. "b")

-- | > mul = y \r a b. a (\pa. add b (r pa b)) zero
mul :: Expr
mul = y :. ("r a b" |-> "a" :. ("pa" |-> add :. "b" :. ("r" :. "pa" :. "b")) :. zero)

-- | > pred' = \a. a id' zero
pred' :: Expr
pred' = "a" |-> "a" :. id' :. zero

toInt :: Expr -> Maybe Int
toInt e = case normalForm (e :. "_suc" :. "_zero") of
    Var "_zero"     -> Just 0
    Var "_suc" :. n -> succ <$> toInt n
    _               -> Nothing

-- | > nil = \c n. n
nil :: Expr
nil = "c n" |-> "n"

-- | > cons = \a b c n. c a b
cons :: Expr
cons = "a b c n" |-> "c" :. "a" :. "b"

list :: [Expr] -> Expr
list = foldr (\e r -> cons :. e :. r) nil

toList :: (Expr -> Maybe a) -> Expr -> Maybe [a]
toList a = go
  where
    go e = case normalForm (e :. "_cons" :. "_nil") of
        Var "_nil"               -> Just []
        Var "_cons" :. ea :. eas -> (:) <$> a ea <*> go eas
        _                        -> Nothing
