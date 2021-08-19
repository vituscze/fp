module Typed.Expr
    (
    -- * Terms
      Name
    , Expr(..)
    , (|->)
    , (.=)

    -- * Types
    , Type(..)

    -- * Type schemes
    , Scheme(..)
    ) where

import Data.List qualified as List
import Data.String

import Expr (Name)

infix  0 .=
infixr 1 :->
infixr 1 :->:
infixr 1 |->
infixl 9 :.

data Expr
    = Var Name               -- ^ Variable
    | Name :-> Expr          -- ^ Abstraction
    | Expr :. Expr           -- ^ Application
    | Let (Name, Expr) Expr  -- ^ Local definition

(.=) :: Name -> Expr -> (Name, Expr)
(.=) = (,)

instance Show Expr where
    showsPrec = go
      where
        -- Extracts the telescope from a lambda expression.
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
instance IsString Expr where
    fromString = Var

-- | Helper function for creating abstraction telescopes.
-- Each bound name must be separated by whitespace.
--
-- > ("x y" |-> Var "x") == ("x" :-> "y" :-> Var "x")
(|->) :: String -> Expr -> Expr
s |-> e = foldr (:->) e $ words s

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

data Scheme = Scheme Int Type

instance Show Scheme where
    showsPrec p (Scheme i t) = showParen (p > 0) (prenex i . shows t)
      where
        prenex 0 = id
        prenex n = showString "∀"
                 . foldr (.) id (List.intersperse (showString " ") $ map tyGenShow [0 .. n - 1])
                 . showString ". "
