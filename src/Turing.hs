-- | This module provides an encoding of Turing machines in lambda calculus.
module Turing
    ( Tape
    , toTape
    , Config
    , toConfig
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

-- | Type of Turing machine tapes. A tape is represented as a triple containing:
--
-- * a list of symbols before the head, in reverse order
-- * a symbol under the head
-- * a list of symbols after the head, in standard order
type Tape = ([Int], Int, [Int])

-- | Converts a Scott-encoded Turing machine tape back into its normal representation.
-- If the expression does not encode a tape, the function returns 'Nothing'.
--
-- >>> toTape (triple :. nil :. nat 2 :. list [nat 1])
-- Just ([],2,[1])
toTape :: Expr -> Maybe Tape
toTape = toTriple (toList toInt) toInt (toList toInt)

-- | Type of Turing machine configurations. A configuration is represented as a pair containing:
--
-- * a state of the machine
-- * a tape
type Config = (Int, Tape)

-- | Converts a Scott-encoded Turing machine configuration back into its normal
-- representation. If the expression does not encode a configuration, the function
-- returns 'Nothing'.
--
-- >>> toConfig (pair :. nat 1 :. (triple :. nil :. nat 2 :. (cons :. nat 1 :. nil)))
-- Just (1,([],2,[1]))
toConfig :: Expr -> Maybe (Int, Tape)
toConfig = toPair toInt toTape

-- | Moves a tape one step to the left, extending the tape with an empty symbol
-- (zero) if necessary.
--
-- >>> toTape (moveLeft :. (triple :. list [nat 0] :. nat 1 :. list [nat 2]))
-- Just ([1,0],2,[])
moveLeft :: Expr
moveLeft = "t" |-> "t" :. ("l x r" |-> "r" :. consCase :. nilCase)
  where
    consCase = "a b" |-> triple :. (cons :. "x" :. "l") :.   "a" :. "b"
    nilCase  =           triple :. (cons :. "x" :. "l") :. nat 0 :. nil

-- | Moves a tape one step to the right, extending the tape with an empty symbol
-- (zero) if necessary.
--
-- >>> toTape (moveRight :. (triple :. list [nat 0] :. nat 1 :. list [nat 2]))
-- Just ([],0,[1,2])
moveRight :: Expr
moveRight = "t" |-> "t" :. ("l x r" |-> "l" :. consCase :. nilCase)
  where
    consCase = "a b" |-> triple :. "b" :.   "a" :. (cons :. "x" :. "r")
    nilCase  =           triple :. nil :. nat 0 :. (cons :. "x" :. "r")

-- | Replaces the symbol under the head with a givevn symbol.
--
-- >>> toTape (writeTape :. nat 1 :. (triple :. nil :. nat 0 :. nil))
-- Just ([],1,[])
writeTape :: Expr
writeTape = "s t" |-> "t" :. ("l x r" |-> triple :. "l" :. "s" :. "r")

-- | Reads the symbol under the head.
--
-- >>> toInt (readTape :. (triple :. nil :. nat 5 :. nil))
-- Just 5
readTape :: Expr
readTape = "t" |-> "t" :. ("l x r" |-> "x")

-- | Performs one step of the Turing machine. A Turing machine step consists of:
--
-- * reading the state
-- * reading the tape
-- * applying the transition function
-- * writing a new symbol to the tape
-- * moving the tape
--
-- In particular, this lambda expression expects two arguments: a transition function,
-- and a configuration.
--
-- The configuration needs to be represented as described in 'Config'.
--
-- The transition function is applied to two arguments: the state of the machine and the
-- current symbol on the tape. Its result should be a triple containing:
--
-- * a symbol to be written to the tape
-- * a movement direction (as a boolean indicating whether to move left)
-- * a state to which the machine transitions
--
-- The result of @turingStep :. trans :. config@ is a configuration.
--
-- If we were to represent this in Haskell types, we would get the following:
--
-- > type State  = Int
-- > type Symbol = Int
-- > turingStep :: ((State, Symbol) -> (Symbol, Bool, State)) -> Config -> Config
turingStep :: Expr
turingStep = "trans config" |-> "config" :. ("state tape" |-> "trans" :. "state" :. (readTape :. "tape") :. handle)
  where
    handle = "sym dir new" |-> pair :. "new" :. ("dir" :. moveLeft :. moveRight :. (writeTape :. "sym" :. "tape"))

-- | Performs 'turingStep's until the machine halts (reaches an end state).
--
-- This lambda expression expects three arguments: an initial configuration, a end state predicate, and
-- a transition function.
--
-- For the representation of the configuration and the transition function, see 'turingStep'. The
-- end state predicate is a function that is applied to the current state and returns a boolean indicating
-- whether the machine should halt.
--
-- The result of @turing :. init :. end :. trans@ is a configuration.
--
-- If we were to represent this in Haskell types, we would get the following:
--
-- > type State  = Int
-- > type Symbol = Int
-- > turing :: Config -> (State -> Bool) -> ((State, Symbol) -> (Symbol, Bool, State)) -> Config
turing :: Expr
turing = "init end trans" |->
    -- Notice that we reduce the step function to normal form. This slightly improves performance.
    -- We cannot normalize the whole expression since the Y combinator has no normal form.
    y :. normalForm ("rec input" |-> "end" :. (fst' :. "input") :. "input" :. ("rec" :. (turingStep :. "trans" :. "input"))) :. "init"
