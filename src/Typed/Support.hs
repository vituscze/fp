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

data TypeError
    = OccursCheck
    | TypeMismatch
    | UnknownVariable
    deriving (Show)

newtype TypeContext
    = TyCtx (Map Name Scheme)
    deriving (Show)

nil :: TypeContext
nil = TyCtx Map.empty

find :: Name -> TypeContext -> Maybe Scheme
find n (TyCtx m) = Map.lookup n m

extend :: Name -> Scheme -> TypeContext -> TypeContext
extend n t (TyCtx m) = TyCtx $ Map.insert n t m
