-- | This modules provides a unification algorithm.
module Typed.Unify
    ( unify
    ) where

import Control.Monad.Except
import Data.Set qualified as Set

import Typed.Expr
import Typed.Free
import Typed.Subst
import Typed.Support

-- | Attempts to unify two types @t1@ and @t2@. If successful, the result is
-- a substitution @s@ such that:
--
-- > apply s t1 == apply s t2
--
-- The function should be used with fully instantiated types. Quantified type variables
-- do not unify.
--
-- >>> unify ("x" :->: "y") ("a" :->: "b") :: Either TypeError Subst
-- Right (fromList [("x",a),("y",b)])
unify :: (MonadError TypeError m) => Type -> Type -> m Subst
unify (TyVar x)    (TyVar y)
    | x == y                    = pure empty
unify (TyVar x)    u
    | x `Set.notMember` free u  = pure $ singleton x u
    | otherwise                 = throwError OccursCheck
unify t            (TyVar y)    = unify (TyVar y) t
unify (t1 :->: t2) (u1 :->: u2) = do
    s1 <- unify           t1            u1
    s2 <- unify (apply s1 t2) (apply s1 u2)
    pure $ s2 @@ s1
unify _            _            = throwError TypeMismatch
