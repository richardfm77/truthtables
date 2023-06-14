module LogicProp (getTable) where

import StackStrings

-- Data structure Boolean expression.
data ExpBool = 
    Var Char | Not ExpBool | Or ExpBool ExpBool |
    And ExpBool ExpBool | Implies ExpBool ExpBool |
    Equal ExpBool ExpBool deriving Show   

-- Analyse syntactically a propositional logic formula.
getExpBool :: String -> ExpBool
getExpBool [p] = 
    if (((fromEnum p) > 64) && ((fromEnum p) < 123) && (p /= 'v'))
        then Var p
        else error "Sintaxis error"
getExpBool ('(':xs) = 
    if ((last xs) == ')')
        then getExpBool (init xs)
        else error "Sintaxis error"
getExpBool form = case (lex form) of
    [("Not", (x:xs))] -> Not (getExpBool xs)
    [("Or", (_:rest))] -> let (exp1, exp2) = clearExpBin rest in 
        Or (getExpBool exp1) (getExpBool exp2)
    [("And", (_:rest))] -> let (exp1, exp2) = clearExpBin rest in 
        And (getExpBool exp1) (getExpBool exp2)
    [("Implies", (_:rest))] -> let (exp1, exp2) = clearExpBin rest in 
        Implies (getExpBool exp1) (getExpBool exp2)
    [("Equal", (_:rest))] -> let (exp1, exp2) = clearExpBin rest in
        Equal (getExpBool exp1) (getExpBool exp2)
    _ -> error "Sintaxis error"      

-- Funtion auxiliar.
clearExpBin :: String -> (String, String)
clearExpBin (x:xs) = 
    if (x == '(') 
        then split (x:xs) [] []
        else 
            let (exp1, (_:rest)) = head (lex (x:xs))
            in (exp1, rest)

-- Funtion auxiliar.
split :: String -> String -> StackString -> (String, String)
split [] _ _ = error "Sintaxis error"
split (x:xs) exp1 stack 
    | x == '(' = split xs (exp1 ++ [x]) (push stack x)
    | x == ')' = split xs (exp1 ++ [x]) (pop stack)
    | otherwise = 
        if (isEmpty stack)
            then (exp1, xs)
            else split xs (exp1 ++ [x]) stack

--
getAtomicsVar :: ExpBool -> [Char]
getAtomicsVar exp = noRepeat (vars exp)

--
vars :: ExpBool -> [Char]
vars Var p vars = 
    if (elem p vars) 
        then []
        else [p]
vars Or exp1 exp2 = (vars exp1) ++ (vars exp1)
vars And exp1 exp2 = (vars exp1) ++ (vars exp1)
vars Implies exp1 exp2 = (vars exp1) ++ (vars exp1)
vars Equal exp1 exp2 = (vars exp1) ++ (vars exp1)

--
noRepeat :: [Char] -> [Char]
noRepeat [] = []
noRepeat (x : xs) =
  if x `elem` xs
    then noRepeat xs
    else x : noRepeat xs

-- evaluate :: ExpBool -> Bool
-- evaluate (Var p b) = b
-- evaluate (Not exp) = not (evaluate exp)
-- evaluate (Or exp1 exp2) = (evaluate exp1) || (evaluate exp2)
-- evaluate (And exp1 exp2) = (evaluate exp1) && (evaluate exp2)
-- evaluate (Implies exp1 exp2) = (not (evaluate exp1)) || (evaluate exp2)
-- evaluate (Equal exp1 exp2) = (evaluate exp1) == (evaluate exp2)

getTable :: String -> String
getTable exp = let expB = getExpBool exp in
    getAtomicsVar expB 