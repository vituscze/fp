-- | This module provides standard representation of expressions in the
-- Hindley-Milner system.
--
-- Expressions can be broken down into two main categories: value-level expressions
-- called terms, and type-level expression called types (and type schemes).
module Typed.Expr
    (
    -- * Terms
      Name
    , Term(..)
    , (|->)

    -- * Types
    , Type(..)

    -- * Type schemes
    , Scheme(..)
    ) where

import Data.List qualified as List
import Data.String

import Expr (Name)

infixr 1 :->
infixr 1 :->:
infixr 1 |->
infixl 9 :.

-- | Standard representation of Hindley-Milner terms. Just like 'Expr.Expr' but with
-- an additional construct: the let-in term.
--
-- > (let f = λx. x in f f) == Let ("f", "x" :-> Var "x") (Var "f" :. Var "f")
data Term
    = Var Name               -- ^ Variable
    | Name :-> Term          -- ^ Abstraction
    | Term :. Term           -- ^ Application
    | Let (Name, Term) Term  -- ^ Local definition

instance Show Term where
    showsPrec = go
      where
        -- Extracts the telescope from a HM term.
        --
        -- Returns a list of bound names and the abstraction body.
        tele (x :-> e) = (x :) <$> tele e
        tele e         = (e, [])

        go _ (Var x)          = showString x
        go p a@(_ :-> _)      = showParen (p > 0)
            ( showString "λ"
            . foldr (.) id (List.intersperse (showString " ") $ map showString vs)
            . showString ". "
            . go 0 b
            )
          where
            (b, vs) = tele a
        go p (e1 :. e2)       = showParen (p > 10)
            ( go 10 e1
            . showString " "
            . go 11 e2
            )
        go p (Let (x, e1) e2) = showParen (p > 0)
            ( showString "let "
            . showString x
            . showString " = "
            . go 0 e1
            . showString " in "
            . go 0 e2
            )

-- This instance, together with OverloadedStrings,
-- allows us to write @"x"@ as a shortcut for @Var "x"@.
instance IsString Term where
    fromString = Var

-- | Helper function for creating abstraction telescopes.
-- Each bound name must be separated by whitespace.
--
-- > ("x y" |-> Var "x") == ("x" :-> "y" :-> Var "x")
(|->) :: String -> Term -> Term
s |-> e = foldr (:->) e $ words s

-- | Standard representation of Hindley-Milner (mono)types.
--
-- > (a → b) == (TyVar "a" :->: TyVar "b")
data Type
    = TyVar Name      -- ^ Free type variables
    | TyGen Int       -- ^ Bound type variables
    | Type :->: Type  -- ^ Function type

-- | Generates a readable representation of bound type variabales.
--
-- >>> tyGenShow 0 ""
-- "t0"
tyGenShow :: Int -> ShowS
tyGenShow i = showString "t" . shows i

instance Show Type where
    showsPrec = go
      where
        go _ (TyVar x)    = showString x
        go _ (TyGen i)    = tyGenShow i
        go p (t1 :->: t2) = showParen (p > 0)
            ( go 1 t1
            . showString " → "
            . go 0 t2
            )

instance IsString Type where
    fromString = TyVar

-- | A representation of Hindley-Milner type schemes (polytypes). Instead of
-- quantifying variables explicitly by their name, we only specify how many
-- quantified variables occur in the type.
--
-- If the scheme quantifies over @n@ variables, the inner type can refer to
-- these variables by using @TyGen i@ where @0 <= i < n@.
--
-- > (∀a. a → b) == Scheme 1 (TyGen 0 :->: TyVar "b")
data Scheme = Scheme Int Type

instance Show Scheme where
    showsPrec p (Scheme i t) = showParen (p > 0) (prenex i . shows t)
      where
        prenex 0 = id
        prenex n = showString "∀"
                 . foldr (.) id (List.intersperse (showString " ") $ map tyGenShow [0 .. n - 1])
                 . showString ". "
