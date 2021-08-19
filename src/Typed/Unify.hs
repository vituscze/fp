module Typed.Unify
    ( unify
    ) where

import Control.Monad.Except
import Data.Set qualified as Set

import Typed.Expr
import Typed.Free
import Typed.Subst
import Typed.Support

unify :: (MonadError TypeError m) => Type -> Type -> m Subst
unify (TyVar x)    (TyVar y)
    | x == y                    = pure empty
unify (TyVar x)    u
    | Set.notMember x (free u)  = pure $ singleton x u
    | otherwise                 = throwError OccursCheck
unify t            (TyVar y)    = unify (TyVar y) t
unify (t1 :->: t2) (u1 :->: u2) = do
    s1 <- unify           t1            u1
    s2 <- unify (apply s1 t2) (apply s1 u2)
    pure $ s2 @@ s1
unify _            _            = throwError TypeMismatch
