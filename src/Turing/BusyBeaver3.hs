-- | This module provides a 3-state busy beaver.
module Turing.BusyBeaver3
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
trans = "state sym" |-> "state" :. ("s" |-> "s" :. (k :. state2) :. state1) :. state0
  where
    state0 = "sym" :. (k :. (triple :. nat 1 :. false :. nat 3)) :. (triple :. nat 1 :. false :. nat 1)
    state1 = "sym" :. (k :. (triple :. nat 1 :. false :. nat 1)) :. (triple :. nat 0 :. false :. nat 2)
    state2 = "sym" :. (k :. (triple :. nat 1 :.  true :. nat 0)) :. (triple :. nat 1 :.  true :. nat 2)

-- | The end state predicate
end :: Expr
end = "state" |-> "state" :. ("s" |-> "s" :. ("ss" |-> "ss" :. (k :. true) :. false) :. false) :. false

-- | 3-state busy beaver
--
-- >>> toConfig machine
-- Just (3,([1,1],1,[1,1,1]))
machine :: Expr
machine = turing :. normalForm initial :. normalForm end :. normalForm trans
