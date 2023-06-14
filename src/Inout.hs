module Inout (inputOutput) where

-- Manage input and output.
inputOutput :: IO ()
inputOutput = do 
    putStrLn "Enter a propositional logic formula:"
    str <- getLine 
    if (str /= "") 
        then putStrLn (filter (/= ' ') str)
        else error "Sintaxis error"