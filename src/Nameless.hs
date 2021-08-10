module Nameless
    ( Expr(..)
    , fromNamed
    , toNamed
    , shift
    , subst
    , normalForm
    ) where

import Control.DeepSeq
import Control.Monad.State
import Data.Map qualified as Map
import Data.Map ((!))

import Expr qualified as N
import Expr (Name)

infixl 9 :.

data Expr
    = BV !Int
    | FV Name
    | Lam Expr
    | Expr :. Expr
    deriving (Show)

instance NFData Expr where
    rnf (BV i)     = rnf i
    rnf (FV x)     = rnf x
    rnf (Lam e)    = rnf e
    rnf (e1 :. e2) = rnf e1 `seq` rnf e2

fromNamed :: N.Expr -> Expr
fromNamed = go 0 Map.empty
  where
    go d m (N.Var v)    = case Map.lookup v m of
        Just i  -> BV (d - i)
        Nothing -> FV v
    go d m (x N.:-> e)  = Lam $ go (d + 1) (Map.insert x (d + 1) m) e
    go d m (e1 N.:. e2) = go d m e1 :. go d m e2

toNamed :: Expr -> N.Expr
toNamed = (`evalState` names) . go 0 Map.empty
  where
    names = map ("_" ++) $ concatMap (`replicateM` ['a' .. 'z']) [1 ..]

    go d m (BV i) = pure $ N.Var $ m ! (d - i)
    go _ _ (FV x) = pure $ N.Var x
    go d m (Lam e) = do
        ~(v:vs) <- get
        put vs
        (v N.:->) <$> go (d + 1) (Map.insert (d + 1) v m) e
    go d m (e1 :. e2) = (N.:.) <$> go d m e1 <*> go d m e2

-- | @shift c e@ shifts all variables in the expression @e@ above the cutoff @c@ by one.
shift :: Int -> Expr -> Expr
shift c (BV i)
    | i < c        = BV i
    | otherwise    = BV (i + 1)
shift _ (FV x)     = FV x
shift c (Lam e)    = Lam $ shift (c + 1) e
shift c (e1 :. e2) = shift c e1 :. shift c e2

subst :: Int -> Expr -> Expr -> Expr
subst n e (BV i)
    | i == n         = e
    | otherwise      = BV i
subst _ _ (FV x)     = FV x
subst n e (Lam f)    = Lam $ subst (n + 1) (shift 0 e) f
subst n e (f1 :. f2) = subst n e f1 :. subst n e f2

bSubst :: Int -> Expr -> Expr -> Expr
bSubst n e (BV i)     = case i `compare` n of
    LT -> BV i
    EQ -> e
    GT -> BV (i - 1)
bSubst _ _ (FV x)     = FV x
bSubst n e (Lam f)    = Lam $ bSubst (n + 1) (shift 0 e) f
bSubst n e (f1 :. f2) = bSubst n e f1 :. bSubst n e f2

whnf :: Expr -> Expr
whnf (BV i)     = BV i
whnf (FV x)     = FV x
whnf (Lam e)    = Lam e
whnf (e1 :. e2) = case whnf e1 of
    Lam e -> whnf $ bSubst 0 e2 e
    e1'   -> e1' :. e2

normalForm :: Expr -> Expr
normalForm (BV i)     = BV i
normalForm (FV x)     = FV x
normalForm (Lam e)    = Lam $ normalForm e
normalForm (e1 :. e2) = case whnf e1 of
    Lam e -> normalForm $ bSubst 0 e2 e
    e1'   -> normalForm e1' :. normalForm e2
