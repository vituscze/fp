-- | This module provides various data types that are used during type inference.
module Typed.Support
    (
    -- * Errors
      TypeError(..)

    -- * Contexts
    , TypeContext(..)
    , nil
    , find
    , extend
    ) where

import Data.Map qualified as Map
import Data.Map (Map)

import Typed.Expr

-- | Errors that can occur during type inference
data TypeError
    = OccursCheck        -- ^ Unification failed due to occurs check
    | TypeMismatch       -- ^ Unification failed due to mismatched types
    | UndefinedVariable  -- ^ Type inference failed due to unbound variable
    deriving (Show)

-- | Typing context of the type inference algorithm
newtype TypeContext
    = TyCtx (Map Name Scheme)
    deriving (Show)

-- | The empty context
nil :: TypeContext
nil = TyCtx Map.empty

-- | Finds the type scheme corresponding to a variable in the given context.
--
-- >>> find "x" (extend "x" (Scheme 1 (TyGen 0)) nil)
-- Just (∀t0. t0)
find :: Name -> TypeContext -> Maybe Scheme
find n (TyCtx m) = Map.lookup n m

-- | Extends a given context with a new variable-scheme pairing.
--
-- >>> extend "x" (Scheme 0 "a") nil
-- TyCtx (fromList [("x",a)])
extend :: Name -> Scheme -> TypeContext -> TypeContext
extend n t (TyCtx m) = TyCtx $ Map.insert n t m
