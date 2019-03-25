module Main(main) where 

import System.Environment 
import Text.Read
import Lib
import Mon

instance Mon Picture where
  m1 = PP []
  (><) p p1 = (&) p p1 

parse :: Picture
parse = PP [((P 0 0), (P 2 2))]


runParser :: Maybe Int -> [IntLine]
runParser (Just scalar) = renderScaled scalar parse

drawPicture :: [IntLine] -> String
drawPicture x = "300 400 translate\n\n" ++ foldr f "" x ++ "\nstroke showpage\n" where 
  f ((x, y), (x1, y1)) acc = show x ++ " " ++ show y ++ " moveto " ++ show x1 ++ " " ++ show y1 ++ " lineto\n"

main = do
    args <- System.Environment.getArgs
    if args == [] then
      putStr $ drawPicture $ runParser $ Just 1
    else
      let scalar = readMaybe (head args) :: Maybe Int in
        if scalar == Nothing || length args /= 1 then
          putStr "Error. Usage: runhaskell Main.hs scalar_int(optional)\n"
        else
          putStr $ drawPicture $ runParser $ scalar
    