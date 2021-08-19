-- | This module provides various encoded data types.
--
-- The Church and Scott encoding coincides for the data types given here,
-- see "Encoding.Church" and "Encoding.Scott" for data types where it does not.
module Encoding
    (
    -- * Booleans
      true
    , false
    , not'
    , and'
    , toBool

    -- * Pairs
    , pair
    , fst'
    , snd'
    , toPair

    -- * Triples
    , triple
    , toTriple

    -- * Choice
    , left
    , right
    , toEither
    ) where

import Prelude hiding (exp)

import Common
import Expr
import Reduce

-- | > true = λx y. x
true :: Expr
true = k

-- | > false = λx y. y
false :: Expr
false = k'

-- | > not' = λa. a false true
not' :: Expr
not' = "a" |-> "a" :. false :. true

-- | > and' = λa b. a b false
and' :: Expr
and' = "a b" |-> "a" :. "b" :. false

-- | Converts an encoded boolean back into its usual representation. If the
-- expression does not encode a boolean, the function returns 'Nothing'.
--
-- >>> toBool (and' :. true :. false)
-- Just False
toBool :: Expr -> Maybe Bool
toBool e = case normalForm (e :. "_true" :. "_false") of
    Var "_true"  -> Just True
    Var "_false" -> Just False
    _            -> Nothing

-- | > pair = λa b f. f a b
pair :: Expr
pair = "a b f" |-> "f" :. "a" :. "b"

-- | > fst' = λp. p true
fst' :: Expr
fst' = "p" |-> "p" :. true

-- | > snd' = λp. p false
snd' :: Expr
snd' = "p" |-> "p" :. false

-- | Converts an encoded pair back into its usual representation. Requires
-- a conversion function for each of the pair elements. If the expression
-- does not encode a pair or if either of the conversion functions fails, the
-- entire function returns 'Nothing'.
--
-- >>> toPair toBool toBool (pair :. true :. false)
-- Just (True,False)
toPair :: (Expr -> Maybe a) -> (Expr -> Maybe b) -> Expr -> Maybe (a, b)
toPair a b e = case normalForm (e :. "_pair") of
    Var "_pair" :. ea :. eb -> (,) <$> a ea <*> b eb
    _                       -> Nothing

-- | > triple = λa b c f. f a b c
triple :: Expr
triple = "a b c f" |-> "f" :. "a" :. "b" :. "c"

-- | Just like 'toPair' but for triples.
--
-- >>> toTriple (toPair toBool toBool) toBool toBool (triple :. (pair :. true :. false) :. true :. false)
-- Just ((True,False),True,False)
toTriple :: (Expr -> Maybe a) -> (Expr -> Maybe b) -> (Expr -> Maybe c) -> Expr -> Maybe (a, b, c)
toTriple a b c e = case normalForm (e :. "_triple") of
    Var "_triple" :. ea :. eb :. ec -> (,,) <$> a ea <*> b eb <*> c ec
    _                               -> Nothing

-- | > left = λa l r. l a
left :: Expr
left = "a l r" |-> "l" :. "a"

-- | > right = λb l r. r b
right :: Expr
right = "b l r" |-> "r" :. "b"

-- | Just like 'toPair' but for disjoint sums.
--
-- >>> toEither toBool toBool (left :. true)
-- Just (Left True)
toEither :: (Expr -> Maybe a) -> (Expr -> Maybe b) -> Expr -> Maybe (Either a b)
toEither a b e = case normalForm (e :. "_left" :. "_right") of
    Var "_left"  :. ea -> Left  <$> a ea
    Var "_right" :. eb -> Right <$> b eb
    _                  -> Nothing
