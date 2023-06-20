module Inout (inputOutput) where

import LogicProp

-- Manage input and output.
inputOutput :: IO ()
inputOutput = do 
    putStrLn "Enter a propositional logic formula:"
    str <- getLine 
    putStrLn ""
    putStr (getTable str)