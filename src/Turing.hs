module Turing
    ( toTape
    , moveLeft
    , moveRight
    , writeTape
    , readTape
    , turingStep
    , turing
    ) where

import Common
import Encoding
import Expr
import Reduce
import Scott

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
turing = "init end trans" |-> y :. normalForm ("rec input" |-> "end" :. (fst' :. "input") :. "input" :. ("rec" :. (turingStep :. "trans" :. "input"))) :. "init"
