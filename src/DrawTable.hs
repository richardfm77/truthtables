module DrawTable (
    drawCells,
    drawLines,
    drawLines2
) where

drawCells :: [Char] -> String
drawCells [] = "|"
drawCells (x:xs) = "| " ++ [x] ++ " " ++ (drawCells xs) 

drawLines :: [Char] -> String
drawLines [] = "\n"
drawLines (_:xs) = "--" ++ (drawLines xs) 

drawLines2 :: [Char] -> String
drawLines2 [] = "\n"
drawLines2 (_:xs) = "----" ++ (drawLines2 xs) 