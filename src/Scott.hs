-- | This module provides various Scott-encoded data types.
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

-- | Converts a natural number into its Scott-encoded representation.
--
-- >>> nat 3
-- (λa s z. s a) ((λa s z. s a) ((λa s z. s a) (λs z. z)))
nat :: Int -> Expr
nat 0 = zero
nat n = suc :. nat (n - 1)

-- | > zero = λs z. z
zero :: Expr
zero = "s z" |-> "z"

-- | > suc = λa s z. s a
suc :: Expr
suc = "a s z" |-> "s" :. "a"

-- | > add = y λr a b. a (λpa. suc (r pa b)) b
add :: Expr
add = y :. ("r a b" |-> "a" :. ("pa" |-> suc :. ("r" :. "pa" :. "b")) :. "b")

-- | > mul = y λr a b. a (λpa. add b (r pa b)) zero
mul :: Expr
mul = y :. ("r a b" |-> "a" :. ("pa" |-> add :. "b" :. ("r" :. "pa" :. "b")) :. zero)

-- | > pred' = λa. a id' zero
pred' :: Expr
pred' = "a" |-> "a" :. id' :. zero

-- | Converts a Scott-encoded natural number back into its usual representation.
-- If the expression does not encode a number, the function returns 'Nothing'.
--
-- >>> toInt (add :. nat 2 :. nat 3)
-- Just 5
toInt :: Expr -> Maybe Int
toInt e = case normalForm (e :. "_suc" :. "_zero") of
    Var "_zero"     -> Just 0
    Var "_suc" :. n -> succ <$> toInt n
    _               -> Nothing

-- | > nil = λc n. n
nil :: Expr
nil = "c n" |-> "n"

-- | > cons = λa b c n. c a b
cons :: Expr
cons = "a b c n" |-> "c" :. "a" :. "b"

-- | Converts a list of expressions into its Scott-encoded representation.
--
-- >>> list [nat 0, nat 1]
-- (λa b c n. c a b) (λs z. z) ((λa b c n. c a b) ((λa s z. s a) (λs z. z)) (λc n. n))
list :: [Expr] -> Expr
list = foldr (\e r -> cons :. e :. r) nil

-- | Converts a Scott-encoded list back into its usual representation. Requires a conversion
-- function for the elements of the list. If the expression does not encode a list or if
-- the conversion function fails, the entire function returns 'Nothing'.
--
-- >>> toList toInt (cons :. nat 2 :. (cons :. nat 1 :. nil))
-- Just [2,1]
toList :: (Expr -> Maybe a) -> Expr -> Maybe [a]
toList a = go
  where
    go e = case normalForm (e :. "_cons" :. "_nil") of
        Var "_nil"               -> Just []
        Var "_cons" :. ea :. eas -> (:) <$> a ea <*> go eas
        _                        -> Nothing
