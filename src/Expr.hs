-- | This module provides a data type for (standard) representation of lambda calculus expressions.
module Expr
    ( Name
    , Expr(..)
    , (|->)
    ) where

import Control.DeepSeq
import qualified Data.List as List
import Data.String

type Name = String

infixr 1 :->
infixr 1 |->
infixl 9 :.

-- | Standard encoding of lambda expressions. Variables and abstractions
-- have explicit names.
--
-- > λx y. y x == "x" :-> "y" :-> Var "y" :. Var "x"
data Expr
    = Var Name       -- ^ Variable
    | Name :-> Expr  -- ^ Abstraction
    | Expr :. Expr   -- ^ Application

instance NFData Expr where
    rnf (Var x)    = rnf x
    rnf (x :-> e)  = rnf x `seq` rnf e
    rnf (e1 :. e2) = rnf e1 `seq` rnf e2

instance Show Expr where
    showsPrec = go
      where
        -- Extracts the telescope from a lambda expression.
        --
        -- Returns a list of bound names and the abstraction body.
        tele (x :-> e) = (x :) <$> tele e
        tele e         = (e, [])

        go _ (Var x)     = (x ++)
        go p a@(_ :-> _) = showParen (p > 0)
            ( ("λ" ++)
            . foldr (.) id (List.intersperse (" " ++) $ map (++) vs)
            . (". " ++)
            . go 0 b
            )
          where
            (b, vs) = tele a
        go p (e1 :. e2)  = showParen (p > 10)
            ( go 10 e1
            . (" " ++)
            . go 11 e2
            )

-- This instance, together with OverloadedStrings,
-- allows us to write @"x"@ as a shortcut for @Var "x"@.
instance IsString Expr where
    fromString = Var

-- | Helper function for creating abstraction telescopes.
-- Each bound name must be separated by whitespace.
--
-- > "x y" |-> Var "x" == "x" :-> "y" :-> Var "x"
(|->) :: String -> Expr -> Expr
s |-> e = foldr (:->) e $ words s
