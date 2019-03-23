module Main(main) where 

import System.Environment 
import Text.Read
-- import Data.Scientific(isInteger)

main = do
    args <- System.Environment.getArgs
    let first = head args
    let scalar = readMaybe first :: Maybe Int
    if scalar == Nothing then
      print "Wrong argument"
    else
      print scalar
    