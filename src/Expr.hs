module Expr
    ( Name
    , Expr(..)
    , (|->)
    ) where

import qualified Data.List as List
import Data.String

type Name = String

infixr 1 :->
infixr 1 |->
infixl 9 :.

data Expr
    = Var Name       -- ^ Variable
    | Name :-> Expr  -- ^ Abstraction
    | Expr :. Expr   -- ^ Application

(|->) :: String -> Expr -> Expr
s |-> e = foldr (:->) e $ words s

prettyPrec :: Int -> Expr -> ShowS
prettyPrec = go
  where
    str  = (++)
    conc = foldr (.) id

    go _ (Var x) = str x
    go p a@(_ :-> _) = showParen (p > 0) . conc $
        [ str "\\"
        , conc . List.intersperse (str " ") . map str $ vs
        , str ". "
        , go 0 b
        ]
      where
        (b, vs) = tele a
        tele (x :-> e) = (x :) <$> tele e
        tele e         = (e, [])
    go p (e1 :. e2) = showParen (p > 10) . conc $
        [ go 10 e1
        , str " "
        , go 11 e2
        ]

instance Show Expr where
    showsPrec = prettyPrec

instance IsString Expr where
    fromString = Var
