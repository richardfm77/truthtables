-- Implementation of Stack data estructure.
module StackStrings
    (
        StackString,
        isEmpty,
        push,
        pop
    ) where

-- Stack Data estructure.
type StackString = [Char]

isEmpty :: StackString -> Bool
isEmpty [] = True
isEmpty _ = False

push :: StackString -> Char -> StackString
push stack c = [c] ++ stack

pop :: StackString -> StackString
pop (x:xs) = xs