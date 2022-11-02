-- | This module provides a 4-state busy beaver.
module Turing.BusyBeaver4
    ( initial
    , trans
    , end
    , machine
    ) where

import Common (k)
import Encoding
import Encoding.Scott
import Expr
import Reduce
import Turing

-- | The initial configuration
initial :: Expr
initial = pair :. nat 0 :. (triple :. nil :. nat 0 :. nil)

-- | The transition function
trans :: Expr
trans = "state sym" |-> "state" :. ("s" |-> "s" :. ("ss" |-> "ss" :. (k :. state3) :. state2) :. state1) :. state0
  where
    state0 = "sym" :. (k :. (triple :. nat 1 :.  true :. nat 1)) :. (triple :. nat 1 :. false :. nat 1)
    state1 = "sym" :. (k :. (triple :. nat 0 :.  true :. nat 2)) :. (triple :. nat 1 :.  true :. nat 0)
    state2 = "sym" :. (k :. (triple :. nat 1 :.  true :. nat 3)) :. (triple :. nat 1 :. false :. nat 4)
    state3 = "sym" :. (k :. (triple :. nat 0 :. false :. nat 0)) :. (triple :. nat 1 :. false :. nat 3)

-- | The end state predicate
end :: Expr
end = "state" |-> "state" :. ("s" |-> "s" :. ("ss" |-> "ss" :. ("sss" |-> "sss" :. (k :. true) :. false) :. false) :. false) :. false

-- | 4-state busy beaver
--
-- >>> toConfig machine
-- Just (4,([1,1,1,1,1,1,1,1,1,1,1,1],0,[1]))
machine :: Expr
machine = turing :. normalForm initial :. normalForm end :. normalForm trans
