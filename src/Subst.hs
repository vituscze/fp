-- | This module provdes tools for generating fresh names and performing substitutions.
module Subst
    (
    -- * Fresh names
    -- $fresh
      Fresh
    , toFresh
    , fromFresh
    , fresh
    , freshest
    -- * Substitutions
    , rename
    , free
    , subst
    ) where

import Control.Monad.State
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Set (Set)

import Expr

-- $fresh
--
-- Generation of fresh names and renaming of bound variables is required
-- to implement capture-avoiding substitution. For simplicity, the expression
-- data types do not distinguish between normal and fresh names. Instead,
-- fresh names use a distinct naming scheme that should not be used for any
-- other purpose.
--
-- Fresh names are based on a numeric index which is then converted into
-- a name. In particular, each fresh name consists of a backtick symbol (@`@)
-- and a sequence of lower-case letters. We use the following mapping:
--
-- >  1  2  3  4 ...  26  27  28 ...
-- > `a `b `c `d ...  `z `aa `ab ...

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

-- | @renameUnsafe src tgt expr@ renames all free occurences of @src@ in the
-- expression @expr@ with the name @tgt@.
--
-- The caller must ensure no free variables are captured.
renameUnsafe :: Name -> Name -> Expr -> Expr
renameUnsafe src tgt = go
  where
    go (Var x)
        | x == src  = Var tgt
        | otherwise = Var x
    go (x :-> e)
        | x == src  = x :-> e
        | otherwise = x :-> go e
    go (e1 :. e2)   = go e1 :. go e2

-- | Renames all free occurences of the name with a fresh name. Returns the
-- fresh name and the renamed expression.
rename :: (Fresh m) => Name -> Expr -> m (Name, Expr)
rename src e = do
    tgt <- fresh
    pure (tgt, renameUnsafe src tgt e)

-- | The set of all free variables in a given expression.
--
-- >>> free ("x" :-> "x" :. "y")
-- fromList ["y"]
free :: Expr -> Set Name
free (Var v)    = Set.singleton v
free (x :-> e)  = free e `Set.difference` Set.singleton x
free (e1 :. e2) = free e1 `Set.union` free e2

-- | @subst name what expr@ substitutes the expression @what@ for
-- all free occurrences of the name @name@ in the expression @expr@,
-- automatically renaming bound names in order to avoid free variable
-- capture.
--
-- > evalState (subst "x" ("y" :. "y") ("x" :. "x")) 0 == (("y" :. "y") :. ("y" :. "y"))
subst :: (Fresh m) => Name -> Expr -> Expr -> m Expr
subst x e = go
  where
    fv = free e

    go (Var y)
        | x == y    = pure e
        | otherwise = pure $ Var y
    go (y :-> f)
        | x == y            = pure $ y :-> f
        | y `Set.member` fv = do
            (y', f') <- rename y f
            (y' :->) <$> go f'
        | otherwise         = (y :->) <$> go f
    go (f1 :. f2) = (:.) <$> go f1 <*> go f2
