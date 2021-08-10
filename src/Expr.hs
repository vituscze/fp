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

instance IsString Expr where
    fromString = Var

(|->) :: String -> Expr -> Expr
s |-> e = foldr (:->) e $ words s
