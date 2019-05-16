import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import AbsGrammar


data Value = 
  VInt Integer 
  | VBool Bool 
  | VString String
  | VFun Ident MyState Block [Value]
  | None
  deriving (Show, Eq)


type Store = Map.Map Ident Value

data MyState = S {
  mmap :: Store
  return_value :: Value,
  was_brk :: Bool
} deriving (Show, Eq)

type Inter  = ErrorT String(StateT MyState Identity)

runInter :: MyState -> Inter a -> (Either String a, MyState)
runInter st ev = runIdentity (runStateT (runErrorT ev) st) 

evalExpr :: Expr -> Inter Value
evalExpr (ELitInt n) = return (VInt n)
evalExpr (ELitTrue) = return (VBool True)
evalExpr (ELitFalse) = return (VBool False)
evalExpr (EVar i@(Ident id)) = do
  st <- Control.Monad.State.get
  case Map.lookup i (mmap st) of
    Nothing -> throwError "Unknown variable - " ++ id
    Just v -> return v
evalExpr (Neg e) = do
  v <- evalExpr e
  case v of 
    (VInt n) -> return (VInt (-n))
    _ -> throwError "Neg must be applied to Int Type"
evalExpr (Not e) = do
  v <- evalExpr e
  case v of
    (VBool b) -> return (VBool !b)
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
        _ -> return (VInt (x / y))
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
    (VInt x, VInt y) -> cmp x y opp
    (VBool b, VBool b1) -> cmp b b1 opp
    _ -> throwError "Comparision operators must be applied to either Ints or Booleans"
evalExpr (ECall i@(Ident id) args@(x:xs)) = do
  



evalStmt :: Stmt -> Inter()
evalStmt (Empty) = do
  return()
evalStmt (Block (x:xs)) = do
  evalStmt x >> evalStmt xs 
evalStmt (Ass i@(Ident id) e) = do
  st <- Control.Monad.State.get
  v <- evalExpr e
  case Map.lookup i (mmap st) of
    Nothing -> throwError "Variable not in scope - " ++ id
    Just x ->  modify(assignValue i v)
evalStmt (Incr i@(Ident id)) = do
  st <- Control.Monad.State.get
  case Map.lookup i (mmap st) of
    Nothing -> throwError "Variable not in scope - " ++ id
    Just x -> modify(assignValue i (x+1))
evalStmt (Decr i@(Ident id)) = do
  st <- Control.Monad.State.get
  case Map.lookup i (mmap st) of
    Nothing -> throwError "Variable not in scope - " ++ id
    Just x -> modify(assignValue i (x-1))
evalStmt (If cond s) = do
  b <- evalExpr cond
  case b of
    (VBool True) -> evalStmt s
    (VBool False) -> return()
    _ -> throwError "Conditon is not a boolean expression"
evalStmt (IfElse cond s s1) = do
  b <- evalExpr cond
  case b of
    (VBool True) -> evalStmt s
    (VBool False) -> evalStmt s1
    _ -> throwError "Condition is not a boolean expression"
evalStmt (While cond s) = do
  b <- evalExpr cond
  case b of
    (VBool True) -> evalStmt s >> evalStmt (While cond s)
    (VBool False) -> return()
    _ -> throwError "Condition is not a boolean expression"
evalStmt (Decl ttype []) = do
  return()
evalStmt (Decl ttype (x:xs)) = do
  case ttype of
    (RType t) -> declSingleVar t x >> evalStmt (Decl ttype xs)
    (FType t) -> declFun t x >> evalStmt (Decl ttype xs) 
  

declSingleVar :: RegType -> Item -> Inter()
declSingleVar t (Init i e) = do
  r <- evalExpr e
  st <- Control.Monad.State.get
  case Map.lookup i (mmap st) of
    Just v -> throwError "This name is already taken"
    Nothing -> modify(addVar i r) >> modify(assignValue i r) 

declFun :: FunType -> Item -> Inter()
declFun (Function t args) (Init i (ELambda tt vars body)) = do
  st <- Control.Monad.State.get
  case Map.Map.lookup i st of
    Just v -> throwError "This name is already taken"
    Nothing -> modify(addVar i) >> modify(addFun i vars body)


cmp :: Ord a => a -> a -> RelOp -> Value
cmp x y opp = case opp of
  LTH -> VBool (x < y)
  LE -> VBool (x <= y)
  GTH -> VBool (x > y)
  GE -> VBool (x >= y)
  EQU -> VBool (x == y)
  NE -> VBool (x /= y)


addFun :: Ident -> [Item] -> Block -> MyState -> MyState
addFun i vars b state = new_state where
  new_state = state {mmap = new_map}
  new_map = Map.update f i (mmap state)
  f x = if x == i then Just (VFun i new_state vars b) else Nothing 

addVar :: Ident -> MyState -> MyState
addVar i state = state {mmap = new_map} where
  new_map = Map.insert i None (mmap state)

assignValue :: Ident -> Value -> MyState -> MyState
assignValue i v state = state {mmap = new_map} where
  new_map = Map.update f i (mmap state)
  f x = if x == i then Just v else Nothing 