-- | This module provdes tools for generating fresh names and performing substitutions.
--
-- Generation of fresh names is required to implement capture-avoiding substitution.
-- For simplicity, the expression data types do not distinguish between normal and
-- fresh names. Instead, fresh names use a distinct naming scheme that should not be
-- used for any other purpose.
--
-- Fresh names are based on a numeric index which is then converted into
-- a name. In particular, each fresh name consists of a backtick symbol (@`@)
-- and a sequence of lower-case letters. We use the following mapping:
--
-- >  1  2  3  4 ...  26  27  28 ...
-- > `a `b `c `d ...  `z `aa `ab ...
module Fresh
    ( Fresh
    , toFresh
    , fromFresh
    , fresh
    , freshest
    ) where

import Control.Monad.State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set

import Expr

-- | Class constraint for computations capable of providing fresh names.
type Fresh m = (MonadState Int m)

-- | Converts a numeric index into a fresh name. The index should be a positive number.
--
-- >>> toFresh 27
-- "`aa"
--
-- If @n@ is positive, then the following identity holds:
--
-- > fromFresh (toFresh n) == n
toFresh :: Int -> String
toFresh = ('`' :) . reverse . go
  where
    go n | n <= 0    = ""
         | otherwise = let r = ((n - 1) `mod` 26) + 1
                       in  toEnum (0x60 + r):go ((n - r) `div` 26)

-- | Converts a fresh name into a numeric index. Returns 0 if the name does not begin
-- with a backtick.
--
-- >>> fromFresh "`aa"
-- 27
--
-- If @x@ is a valid fresh name, then the following identity holds:
--
-- > toFresh (fromFresh x) == x
--
-- Note that the function does not attempt to recognize and handle names such as @`A@ or @`&@.
fromFresh :: String -> Int
fromFresh ('`':r) = List.foldl' (\a n -> n + 26 * a) 0 $ map (subtract 0x60 . fromEnum) r
fromFresh _       = 0

-- | Generates a fresh name.
fresh :: (Fresh m) => m Name
fresh = state $ \s -> (toFresh s, s + 1)

-- | Finds the most recent fresh variable.
--
-- >>> freshest ("`a" :-> "`b" :. "`a")
-- 2
--
-- >>> freshest ("x" :-> "x")
-- 0
freshest :: Expr -> Int
freshest = Maybe.fromMaybe 0 . Set.lookupMax . Set.map fromFresh . go
  where
    go (Var v)    = Set.singleton v
    go (x :-> e)  = go e `Set.union` Set.singleton x
    go (e1 :. e2) = go e1 `Set.union` go e2
