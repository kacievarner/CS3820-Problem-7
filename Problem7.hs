{-# LANGUAGE FlexibleContexts #-}
module Problem7 where

import Control.Monad
import Control.Monad.State
import Control.Monad.RWS (MonadState)

--------------------------------------------------------------------------------
-- Some test data

macbeth :: String
macbeth =
    "She should have died hereafter;\n\
    \There would have been a time for such a word.\n\
    \-- To-morrow, and to-morrow, and to-morrow,\n\
    \Creeps in this petty pace from day to day,\n\
    \To the last syllable of recorded time;\n\
    \And all our yesterdays have lighted fools\n\
    \The way to dusty death.\n\
    \Out, out, brief candle!\n\
    \Life is but a walking shadow, a poor player\n\
    \That struts and frets his hour upon the stage\n\
    \And then is heard no more. It is a tale\n\
    \Told by an idiot, full of sound and fury\n\
    \Signifying nothing."

fiftyFour = 
    "I like a look of agony,\n\
    \Because I know it's true --\n\
    \Men do not sham Convulsion,\n\
    \Nor simulate, a Throe --\n\
    \\n\
    \The Eyes glaze once -- and that is death\n\
    \Impossible to feign\n\
    \The Beads upon the Forehead\n\
    \By homely Anguish strung."    


{-------------------------------------------------------------------------------

CS:3820 Fall 2021 Problem of the Week, Week 7
=============================================

This problem concerns using monads.  In particular, we're going to focus on the
state and IO monads.  The *state monad* encapsulates stateful operations---that
is to say, operations that can both read from and write to some implicit shared
value.  The *IO* monad captures input/output operations, such as reading from or
writing to files or the console.  

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Problem 7-1
-----------

For the first part, you need to write an operation, in the state monad, that
adds a running character total to a string.  The idea is basically that, given
an input string like

    Men do not sham Convulsion,

We want to produce an output string like

    Men do not sham Convulsion,                                                 (27)

where:

 - 27 is the running total number of characters after this line
 - the count is *right*-aligned at column 80

So, the output string before would be correct *if it were the first string*.
However, if we'd already seen 52 characters, then the correct output string
would be

    Men do not sham Convulsion,                                                 (79)

The challenge, of course, is knowing how many characters we'd seen already---and
recording the new total going forward.  This is where the *state* monad comes
in.  In addition to the normal monad operations (`return` and `>>=`, and `do`
notation), state monads provide operations `get` and `put`.  The `get` operation
retrieves a value stored within the monad, while the `put` value updates it.

Your `addConst` function will live in a monad where the only state is the
running total of characters.  Your function should not just insert the new
running total in the returned string, but also update the running total in the
monad.

The type of `addCount` is *parametric* over an arbitrary monad that satisfies
the `MonadState Int` type class.  This ensures that *all* you can rely on are
the monad and state operations, not any details of a particular implementation
of the state monad.

The `Data.Text` module includes functions that might seem to help with
right-aligning the count; while you can use them, if you try hard enough, it's
easier to just use the list functions we've seen already.  Don't forget about
`replicate`!  You may assume that no line will be long enough that the text of
the line and the running total would overlap.

-------------------------------------------------------------------------------}

addCount :: MonadState Int m => String -> m String
addCount stringSum = do
             totalLength <- get --Retrieves the value stored at the variable
             --Variables to establish a new total length
             let currentLength = length stringSum --Sets current length equal to the length at variable s
             if currentLength == 0 then --Base case for if the current length of the string/line is equal to 0
                 return stringSum --Then return current length of 0
             else do --If value is anything except for 0
                let newLength = currentLength + totalLength --Takes current and total length and combines them to produce a new total length
                --Updates the value
                put newLength --Updates the value of the new length
                --Calculates the final line/string length and formats it
                let rightLength = length ("(" ++ show newLength ++ ")") --Creates a new new length for producting the first length in the output
                let x = 80 + totalLength - (newLength + rightLength) --Creates a variable to store the new final length and accounts for formatting(redundant sentence)
                --Return formatted output
                return (stringSum ++ replicate x ' ' ++ "(" ++ show newLength ++ ")") --Replicates sentence stored at x and returns the final length of the string

-- >>> runState (addCount "If i cant get a job and learned haskell for nothing i will cry;") 0 
-- ("If i cant get a job and learned haskell for nothing i will cry;             (63)",63)

-- >>> runState (addCount "My love for haskell is non-existant;") 10     
-- ("My love for haskell is non-existant;                                        (46)",46)

-- >>> runState (addCount "She should have died hereafterrrr;") 0
-- ("She should have died hereafterrrr;                                          (34)",34)
-- >>> runState (addCount "She should have died hereafter;") 100
-- ("She should have died hereafter;                                            (131)",131)

{-------------------------------------------------------------------------------

Problem 7-2
-----------

For this problem, you should write a function that adds the running total to a
*list* of strings.  Of course, you should use your solution to part 1.  However,
there's one additional catch: you should only print the running total on lines
that aren't blank!  (You don't have to worry about non-printing characters: a
line of spaces or tab characters is not blank.)  You might find the `mapM`
function, of type

    Monad m => (a -> m b) -> [a] -> m [b]

helpful.

-------------------------------------------------------------------------------}

addCounts :: MonadState Int m => [String] -> m [String]
addCounts = mapM addCount --Use mapM to filter out empty strings/lines
               
--Success can be seen in the example below where the newline is filtered out

-- >>> runState (addCounts (take 5 (lines fiftyFour))) 0
-- (["I like a look of agony,                                                     (23)","Because I know it's true --                                                 (50)","Men do not sham Convulsion,                                                 (77)","Nor simulate, a Throe --                                                   (101)",""],101)

-- >>> runState (addCounts (take 2 (lines macbeth))) 0
-- (["She should have died hereafter;                                             (31)","There would have been a time for such a word.                               (76)"],76)


{-------------------------------------------------------------------------------

Problem 7-3
-----------

Finally, you should write a function that, given an input string, prints that
string to standard output with running character totals.  For example, given the
poem above, your function should behave as follows:

*Problem7> printLines fiftyFour 
I like a look of agony,                                                     (23)
Because I know it's true --                                                 (50)
Men do not sham Convulsion,                                                 (77)
Nor simulate, a Throe --                                                   (101)

The Eyes glaze once -- and that is death                                   (141)
Impossible to feign                                                        (160)
The Beads upon the Forehead                                                (187)
By homely Anguish strung.                                                  (212)

Because your solution runs in the IO monad, you won't be able to execute it in a
comment within this file.  Instead, you'll need to use the Haskell interpreter
from the command line.  Give the command `cabal repl` to start the interpreter
with this file loaded.

You may find the following functions helpful:
    putStrLn :: String -> IO ()
    -- prints the input string, followed by a newline
    mapM_    :: Monad m => (a -> m b) -> [a] -> m ()
    -- maps a monadic function across a list, discarding the results
    runState :: MonadState s m => m a -> s -> (a, s)
    -- executes a computation in the state monad, requiring an initial state 
    -- and returning the final result and the final state.

-------------------------------------------------------------------------------}

printLines :: String -> IO () --Prints the entire formatted string/lines
printLines formattedString = mapM_ putStrLn(evalState(addCounts(lines formattedString)) 0) --Uses monad function to print input string
                                                                                           --evaluate the state, then adds the count of the line
                                                                                           --Starts at index length of 0
