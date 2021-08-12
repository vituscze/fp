-- | This module provides various Church-encoded data types.
module Church
    (
    -- * Natural numbers
      nat
    , zero
    , suc
    , add
    , add'
    , mul
    , mul'
    , exp
    , exp'
    , pred'
    , toInt
    ) where

import Prelude hiding (exp)

import Common
import Encoding
import Expr
import Reduce

-- | Converts a natural number into its Church-encoded representation.
--
-- >>> nat 3
-- λf x. f (f (f x))
nat :: Int -> Expr
nat i = "f x" |-> go i
  where
    go 0 = "x"
    go n = "f" :. go (n - 1)

-- | > zero = λf x. x
zero :: Expr
zero = "f x" |-> "x"

-- | > suc = λa f x. f (a f x)
suc :: Expr
suc = "a f x" |-> "f" :. ("a" :. "f" :. "x")

-- | > add = λa b. a suc b
add :: Expr
add = "a b" |-> "a" :. suc :. "b"

-- | More efficient version of 'add'.
--
-- > add' = λa b f x. a f (b f x)
add' :: Expr
add' = "a b f x" |-> "a" :. "f" :. ("b" :. "f" :. "x")

-- | > mul = λa b. a (add b) 0
mul :: Expr
mul = "a b" |-> "a" :. (add :. "b") :. zero

-- | More efficient version of 'mul'.
--
-- > mul' = λa b f. a (b f)
mul' :: Expr
mul' = "a b f" |-> "a" :. ("b" :. "f")

-- | > exp = λa b. b (mul a) 1
exp :: Expr
exp = "a b" |-> "b" :. (mul :. "a") :. (suc :. zero)

-- | More efficient version of 'exp'.
--
-- > exp' = λa b. b a
exp' :: Expr
exp' = "a b" |-> "b" :. "a"

-- | Converts a Church-encoded natural number back into its usual representation.
-- If the expression does not encode a number, the function returns 'Nothing'.
--
-- >>> toInt (add :. nat 2 :. nat 3)
-- Just 5
toInt :: Expr -> Maybe Int
toInt e = go $ normalForm $ e :. "_suc" :. "_zero"
  where
    go (Var "_zero")     = Just 0
    go (Var "_suc" :. n) = succ <$> go n
    go _                 = Nothing

-- | The predecessor function, implemented using pair encoding.
--
-- @
-- pred = λa. snd' (a step base)
--   where
--     step = λp. pair true (fst' p suc id' (snd' p))
--     base = pair false 0
-- @
pred' :: Expr
pred' = "a" |-> snd' :. ("a" :. step :. base)
  where
    step = "p" |-> pair :. true :. (fst' :. "p" :. suc :. id' :. (snd' :. "p"))
    base = pair :. false :. nat 0
