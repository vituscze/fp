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

-- | > add' = λa b f x. a f (b f x)
add' :: Expr
add' = "a b f x" |-> "a" :. "f" :. ("b" :. "f" :. "x")

-- | > mul = λa b. a (add b) 0
mul :: Expr
mul = "a b" |-> "a" :. (add :. "b") :. zero

-- | > mul' = λa b f. a (b f)
mul' :: Expr
mul' = "a b f" |-> "a" :. ("b" :. "f")

-- | > exp = λa b. b (mul a) 1
exp :: Expr
exp = "a b" |-> "b" :. (mul :. "a") :. (suc :. zero)

-- | > exp' = λa b. b a
exp' :: Expr
exp' = "a b" |-> "b" :. "a"

toInt :: Expr -> Maybe Int
toInt e = go $ normalForm $ e :. "_suc" :. "_zero"
  where
    go (Var "_zero")     = Just 0
    go (Var "_suc" :. n) = succ <$> go n
    go _                 = Nothing

-- |
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
