module Main where

import System.Environment(getArgs)
import System.IO.Error (userError,ioError)

import LexGrammar
import ParGrammar
import AbsGrammar
import ErrM
import Interpreter


main :: IO ()
main = do
  file <- getArgs >>= getFilename
  prog <- readFile file 
  case pBlock $ myLexer prog of
    (Ok p) -> case interpret p of
      (Left err) -> putStrLn err
      (Right state) -> showOutput state
      -- (Right state) -> mapM_ print (store state) 
    (Bad err) -> ioError (userError err)

getFilename :: [String] -> IO String
getFilename [] = ioError (userError "No filename")
getFilename (x:xs) = return x

showOutput :: MyState -> IO()
showOutput st = do
  mapM_ print $ reverse (output st)