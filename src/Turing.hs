module Turing
    (
    -- * Turing machines
      initial
    , toTape
    , moveLeft
    , moveRight
    , writeTape
    , readTape
    , turingStep
    , turing

    -- * 2-state busy beaver
    , busyBeaverEnd
    , busyBeaverTrans
    ) where

import Common
import Encoding
import Expr
import Scott

initial :: Expr
initial = triple :. nil :. nat 0 :. nil

toTape :: Expr -> Maybe ([Int], Int, [Int])
toTape = toTriple (toList toInt) toInt (toList toInt)

moveLeft :: Expr
moveLeft = "t" |-> "t" :. ("l x r" |-> "r" :. consCase :. nilCase)
  where
    consCase = "a b" |-> triple :. (cons :. "x" :. "l") :.   "a" :. "b"
    nilCase  =           triple :. (cons :. "x" :. "l") :. nat 0 :. nil

moveRight :: Expr
moveRight = "t" |-> "t" :. ("l x r" |-> "l" :. consCase :. nilCase)
  where
    consCase = "a b" |-> triple :. "b" :.   "a" :. (cons :. "x" :. "r")
    nilCase  =           triple :. nil :. nat 0 :. (cons :. "x" :. "r")

writeTape :: Expr
writeTape = "s t" |-> "t" :. ("l x r" |-> triple :. "l" :. "s" :. "r")

readTape :: Expr
readTape = "t" |-> "t" :. ("l x r" |-> "x")

turingStep :: Expr
turingStep = "trans input" |-> "input" :. ("state tape" |-> "trans" :. "state" :. (readTape :. "tape") :. handle)
  where
    handle = "sym dir new" |-> pair :. "new" :. ("dir" :. moveLeft :. moveRight :. (writeTape :. "sym" :. "tape"))

turing :: Expr
turing = "end trans" |-> y :. ("rec input" |-> "end" :. (fst' :. "input") :. "input" :. ("rec" :. (turingStep :. "trans" :. "input"))) :. (pair :. nat 0 :. initial)

busyBeaverTrans :: Expr
busyBeaverTrans = "state sym" |-> "state" :. ("s" |-> "s" :. id' :. state1) :. state0
  where
    state0 = "sym" :. ("s" |-> triple :. nat 1 :.  true :. nat 1) :. (triple :. nat 1 :. false :. nat 1)
    state1 = "sym" :. ("s" |-> triple :. nat 1 :. false :. nat 2) :. (triple :. nat 1 :.  true :. nat 0)

busyBeaverEnd :: Expr
busyBeaverEnd = "state" |-> "state" :. ("s" |-> "s" :. (k :. true) :. false) :. false
