import LogicProp

main :: IO ()
main = do 
    putStrLn ""
    print (getExpBool "Implies (Not q) (Not p)")
