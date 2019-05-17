module Interpreter where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import AbsGrammar


data Value = 
  VInt Integer 
  | VBool Bool 
  | VString String
  | VFun Ident [Var] Block
  | None
  deriving (Show, Eq)

type Loc = Int

type Store = Map.Map Loc Value
type Env = Map.Map Ident Loc

data MyState = S {
  store :: Store,
  env :: Env,
  output :: [String]
} deriving (Show, Eq)

type Inter  = ErrorT String(StateT MyState Identity)

runInter :: MyState -> Inter a -> (Either String a, MyState)
runInter st ev = runIdentity (runStateT (runErrorT ev) st) 

initialState :: MyState
initialState = S Map.empty Map.empty []

interpret :: Block -> Either String MyState
interpret p = case runInter initialState (evalBlock p) of
  ((Left err),_) -> Left $ "Error " ++ err
  (_,state) -> Right state


evalExpr :: Expr -> Inter Value
evalExpr (ELitInt n) = return (VInt n)
evalExpr (ELitTrue) = return (VBool True)
evalExpr (ELitFalse) = return (VBool False)
evalExpr (EString str) = return (VString str)
evalExpr (EVar i@(Ident id)) = do
  st <- Control.Monad.State.get
  case Map.lookup i (env st) of
    Nothing -> throwError $ "Unknown variable - " ++ id
    Just l -> return (getValue l (store st))
evalExpr (Neg e) = do
  v <- evalExpr e
  case v of 
    (VInt n) -> return (VInt (-n))
    _ -> throwError "Neg must be applied to Int Type"
evalExpr (Not e) = do
  v <- evalExpr e
  case v of
    (VBool b) -> return (VBool $ not b)
    _ -> throwError "Not must be applied to Boolean Type"
evalExpr (EAdd e opp e1) = do
  v <- evalExpr e
  v1 <- evalExpr e1
  case (v, v1) of
    (VInt x, VInt y) -> case opp of
      Plus -> return (VInt (x + y))
      Minus -> return (VInt (x - y))
    _ -> throwError "Arithmetic operator must be applied to Ints"
evalExpr (EMul e opp e1) = do
  v <- evalExpr e
  v1 <- evalExpr e1
  case (v, v1) of 
    (VInt x, VInt y) -> case opp of
      Times -> return (VInt (x * y))
      Div -> case y of
        0 -> throwError "Division by 0"
        _ -> return (VInt (quot x y))
      Mod -> case y of
        0 -> throwError "Division by 0"
        _ -> return (VInt (x `mod` y))
    _ -> throwError "Arithmetic operator must be applied to Ints"
evalExpr (EAnd e e1) = do
  v <- evalExpr e
  v1 <- evalExpr e1
  case (v, v1) of
    (VBool b, VBool b1) -> return (VBool (b && b1))
    _ -> throwError "Logical AND must be applied to Bool Types"
evalExpr (EOr e e1) = do
  v <- evalExpr e
  v1 <- evalExpr e1
  case (v, v1) of
    (VBool b, VBool b1) -> return (VBool (b || b1))
    _ -> throwError "Logical OR must be applied to Bool Types"
evalExpr (ERel e opp e1) = do
  v <- evalExpr e
  v1 <- evalExpr e1
  case (v, v1) of
    (VInt x, VInt y) -> return $ cmp x y opp
    (VBool b, VBool b1) -> return $ cmp b b1 opp
    (VString s, VString s1) -> return $ cmp s s1 opp
    _ -> throwError "Comparision operators must be applied to either Ints, Booleans or Strings"


evalStmt :: Stmt -> Inter()
evalStmt (Empty) = do
  return()
evalStmt (BStmt (Block (x:xs))) = do
  evalStmt x >> evalBlock (Block xs) 
evalStmt (Ass i@(Ident id) e) = do
  st <- Control.Monad.State.get
  v <- evalExpr e
  case Map.lookup i (env st) of
    Nothing -> throwError $ "Variable not in scope - " ++ id
    Just l ->  modify(assignValue l v)
evalStmt (Incr i@(Ident id)) = do
  st <- Control.Monad.State.get
  let f x = VInt (x + 1) in incrOrDecr f i st
evalStmt (Decr i@(Ident id)) = do
  st <- Control.Monad.State.get
  let f x = VInt (x - 1) in incrOrDecr f i st
evalStmt (If cond s) = do
  b <- evalExpr cond
  case b of
    (VBool True) -> evalBlock s
    (VBool False) -> return()
    _ -> throwError "Conditon is not a boolean expression"
evalStmt (IfElse cond s s1) = do
  b <- evalExpr cond
  case b of
    (VBool True) -> evalBlock s
    (VBool False) -> evalBlock s1
    _ -> throwError "Condition is not a boolean expression"
evalStmt (While cond s) = do
  b <- evalExpr cond
  case b of
    (VBool True) -> evalBlock s >> evalStmt (While cond s)
    (VBool False) -> return()
    _ -> throwError "Condition is not a boolean expression"
evalStmt (Decl ttype []) = do
  return()
evalStmt (Decl ttype (x:xs)) = do
  case ttype of
    (RType t) -> declSingleVar t x >> evalStmt (Decl ttype xs)
    (FType t) -> declFun t x >> evalStmt (Decl ttype xs)
evalStmt (Prnt i@(Ident id)) = do
  st <- Control.Monad.State.get
  case Map.lookup i (env st) of
    Nothing -> throwError "Unknown variable"
    Just l -> do
      let (Just v) = Map.lookup l (store st) 
        in modify(addToOutput v)
evalStmt (PrntLit e) = do
  v <- evalExpr e
  modify(addToOutput v)
evalStmt (Call i@(Ident id) args) = do
  return()
  st <- Control.Monad.State.get
  -- case e of
    -- (EVar i@(Ident id) -> do
  case Map.lookup i (env st) of
    Nothing -> throwError $ ("Function " ++ id ++ " does not exist")
    Just l -> do
      case Map.lookup l (store st) of
        Just (VFun i vars body) -> applyArgs args vars (env st) >> evalBlock body 
        -- >> modify(revertState st)
        _ -> throwError $ id ++ " is not a function"
    -- _ -> throwError "Not supported"
evalStmt _ = throwError "Asa"
  

evalBlock :: Block -> Inter()
evalBlock (Block []) = return()
evalBlock (Block (x:xs)) = evalStmt x >> evalBlock (Block xs)


declSingleVar :: RegType -> Item -> Inter()
declSingleVar t (Init i e) = do
  r <- evalExpr e
  st <- Control.Monad.State.get
  let loc = (Map.size (env st)) + 1 
    in case Map.lookup i (env st) of
      Just v -> throwError "This name is already taken"
      Nothing -> do 
        modify(addVar i)
        modify(assignValue loc r) 

declFun :: FunType -> Item -> Inter()
declFun (Function t args) (Init i e) = do
  st <- Control.Monad.State.get
  let loc = (Map.size (env st)) + 1 
    in case e of
      (ELambda tt vars body) -> do
        case Map.lookup i (env st) of
          Just v -> throwError "This name is already taken"
          Nothing -> do
            modify(addVar i)
            modify(assignValue loc (VFun i vars body)) 
            -- modify(addFun i loc vars body)
      _ -> throwError "Expresion is not a function"

incrOrDecr :: (Integer -> Value) -> Ident -> MyState -> Inter()
incrOrDecr f i@(Ident id) st = do
  case Map.lookup i (env st) of
    Nothing -> throwError $ "Variable not in scope" ++ id
    Just l -> do 
      case getValue l (store st) of
        (VInt x) -> let val = f x in modify(assignValue l val)
        _ -> throwError "Increment operator can be use only with numerical types"

cmp :: Ord a => a -> a -> RelOp -> Value
cmp x y opp = case opp of
  LTH -> VBool (x < y)
  LE -> VBool (x <= y)
  GTH -> VBool (x > y)
  GE -> VBool (x >= y)
  EQU -> VBool (x == y)
  NE -> VBool (x /= y)

applyArgs :: [Expr] -> [Var] -> Env -> Inter()         
applyArgs (a:as) ((Var t i):xs) env = do
  v <- evalExpr a
  case Map.lookup i env of
    Nothing -> do
      let loc = (Map.size env + 1) 
      modify(addVar i) >>  modify(assignValue loc v) >> applyArgs as xs env
    _ -> throwError "Ambigious reference"
applyArgs [] [] _ = return()
applyArgs _ _ _ = throwError "Number of function arguments is incorrect"

getValue :: Loc -> Store -> Value
getValue l s = case Map.lookup l s of
  Nothing -> None
  Just v -> v

-- addFun :: Ident -> Loc -> [Var] -> Block -> MyState -> MyState
-- addFun i l vars b state = state {store = new_map} where
--   new_map = Map.insert f l (store state)
--   f = VFun i vars b 

addVar :: Ident -> MyState -> MyState
addVar i state = state {env = new_env} where
  new_env = Map.insert i loc (env state)
  loc = (Map.size (env state)) + 1 

assignValue :: Loc -> Value -> MyState -> MyState
assignValue l v state = state {store = new_store} where
  new_store = Map.insert l v (store state)


addToOutput :: Value -> MyState -> MyState
addToOutput v state = state {output = es : (output state)} where
  es = case v of
    (VInt x) -> show x
    (VBool b) -> show b
    (VString ss) -> show ss

revertState :: MyState -> MyState -> MyState
revertState old_state state = old_state