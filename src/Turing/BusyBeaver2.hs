module Turing.BusyBeaver2
    ( initial
    , trans
    , end
    , machine
    ) where

import Common (k)
import Encoding
import Expr
import Reduce
import Scott
import Turing

initial :: Expr
initial = pair :. nat 0 :. (triple :. nil :. nat 0 :. nil)

trans :: Expr
trans = "state sym" |-> "state" :. (k :. state1) :. state0
  where
    state0 = "sym" :. (k :. (triple :. nat 1 :.  true :. nat 1)) :. (triple :. nat 1 :. false :. nat 1)
    state1 = "sym" :. (k :. (triple :. nat 1 :. false :. nat 2)) :. (triple :. nat 1 :.  true :. nat 0)

end :: Expr
end = "state" |-> "state" :. ("s" |-> "s" :. (k :. true) :. false) :. false

machine :: Expr
machine = turing :. normalForm initial :. normalForm end :. normalForm trans
