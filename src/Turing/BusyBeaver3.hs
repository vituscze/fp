module Turing.BusyBeaver3
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
trans = "state sym" |-> "state" :. ("s" |-> "s" :. (k :. state2) :. state1) :. state0
  where
    state0 = "sym" :. ("s" |-> triple :. nat 1 :. false :. nat 3) :. (triple :. nat 1 :. false :. nat 1)
    state1 = "sym" :. ("s" |-> triple :. nat 1 :. false :. nat 1) :. (triple :. nat 0 :. false :. nat 2)
    state2 = "sym" :. ("s" |-> triple :. nat 1 :.  true :. nat 0) :. (triple :. nat 1 :.  true :. nat 2)

end :: Expr
end = "state" |-> "state" :. ("s" |-> "s" :. ("ss" |-> "ss" :. (k :. true) :. false) :. false) :. false

machine :: Expr
machine = turing :. normalForm initial :. normalForm end :. normalForm trans
