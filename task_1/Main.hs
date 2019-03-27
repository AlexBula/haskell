module Main(main) where 

import System.Environment
import Text.Read 
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad(liftM)
import Data.Maybe(fromJust)
import Lib
import Mon


data MyState = S {
  stack :: [R], 
  base_point :: Maybe Point,
  current_point :: Maybe Point,
  picture :: [(Point, Point)], 
  transformation :: Transform,
  path_length :: Int
}

type ParseM a = ErrorT String(StateT MyState Identity) a

runParseM :: MyState -> ParseM a -> (Either String a, MyState)
runParseM st ev = runIdentity (runStateT (runErrorT ev) st)

addM :: ParseM ()
addM = do
  y <- pop
  x <- pop
  push (x + y)

subM :: ParseM()
subM = do
  y <- pop
  x <- pop
  push (x - y)

mulM :: ParseM()
mulM = do
  y <- pop
  x <- pop
  push (x * y)

divM :: ParseM()
divM = do
  y <- pop
  x <- pop
  if y == 0 then
    throwError "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"
  else
    push (x / y)

moveToM :: ParseM()
moveToM = do
  y <- pop
  x <- pop
  modify(f x y) where
    f x y state = state {base_point = Just (trpoint trans (P x y)), 
                        current_point = Just (trpoint trans (P x y)), 
                        path_length = 0} where
      trans = transformation state

drawLine :: R -> R -> MyState -> MyState
drawLine x y state = state {picture = (new_line:pic), 
                            current_point = Just new_point,
                            path_length = incremented} where
  pic = picture state
  trans = transformation state
  new_point = trpoint trans (P x y)
  new_line = (fromJust (current_point state), new_point)
  incremented = (path_length state) + 1

lineToM :: ParseM()
lineToM = do
  y <- pop
  x <- pop
  modify (drawLine x y)

-- closePathM :: ParseM()
-- closePathM = do
--   state <- Control.Monad.State.get
--   if (path_length state) >= 2 then
--     let Just (P x y) = base_point state in modify (drawLine x y)
--   else 
--     return

addTransformation :: Transform -> MyState -> MyState
addTransformation t state = state {transformation = new_trans} where
    new_trans = (><) t (transformation state)

translateM :: ParseM()
translateM = do
  y <- pop
  x <- pop
  modify(addTransformation $ Translate (V x y))

rotateM :: ParseM()
rotateM = do
  x <- pop
  modify(addTransformation $ Rotate x)

addHead :: R -> MyState -> MyState
addHead x state = state {stack = (x:xs)} where
  xs = stack state

push :: R -> ParseM ()
push x = do 
  modify (addHead x)

removeHead :: MyState -> MyState
removeHead state = state {stack = xs} where
  xs = tail $ stack state

pop :: ParseM R
pop = do
  state <- Control.Monad.State.get
  let s = stack state
  if s == [] then
    throwError "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"
  else
    do
      let val = head s
      modify(removeHead)  
      return $ val


-- parseInput :: [String] -> ParseM()
-- parseInput (x:xs) = do
--   v <- readMaybe x :: Maybe R
--   if v == Nothing then
--     case x of
--       "add" -> addM
--       "sub" -> subM
--       "mul" -> mulM
--       "div" -> divM
--       "moveto" -> moveToM
--       "lineto" -> lineToM
--       "closepath" -> closePathM
--       "translate" -> translateM
--       "rotate" -> rotateM
--       _ -> throwError "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"
--   else
--     push v


parse :: Picture
parse = PP [((P 0 0), (P 2 2)), ((P 2 2), (P 4 4))]

runParser :: Maybe Int -> [IntLine]
runParser (Just scalar) = renderScaled scalar parse

drawPicture :: [IntLine] -> String
drawPicture x = "300 400 translate\n\n" ++ foldr f "" x ++ "\nstroke showpage\n" where 
  f ((x, y), (x1, y1)) acc = show x ++ " " ++ show y ++ " moveto " ++ show x1 ++ " " ++ show y1 ++ " lineto\n" ++ acc

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
    