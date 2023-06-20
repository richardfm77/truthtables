import LogicProp

main :: IO ()
main = do 
    
    putStrLn ""
    putStr (getTable "Implies (And p q) (And (And s (Not r)) t)")