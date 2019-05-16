{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintGrammar where

-- pretty-printer generated by the BNF converter

import AbsGrammar
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print Block where
  prt i e = case e of
    Block stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print Stmt where
  prt i e = case e of
    Empty -> prPrec i 0 (concatD [doc (showString "To Be Empty Or Not To Be Empty")])
    BStmt block -> prPrec i 0 (concatD [doc (showString "{"), prt 0 block, doc (showString "}")])
    Decl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items])
    Ass id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 expr])
    Incr id -> prPrec i 0 (concatD [prt 0 id, doc (showString "++")])
    Decr id -> prPrec i 0 (concatD [prt 0 id, doc (showString "--")])
    Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr])
    VRet -> prPrec i 0 (concatD [doc (showString "return")])
    Brk -> prPrec i 0 (concatD [doc (showString "break")])
    Cont -> prPrec i 0 (concatD [doc (showString "continue")])
    If expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 block, doc (showString "}")])
    IfElse expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 block1, doc (showString "}"), doc (showString "else"), doc (showString "{"), prt 0 block2, doc (showString "}")])
    While expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 block, doc (showString "}")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Item where
  prt i e = case e of
    NoInit id -> prPrec i 0 (concatD [prt 0 id])
    Init id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 expr])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Var where
  prt i e = case e of
    Var type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Type where
  prt i e = case e of
    RType regtype -> prPrec i 0 (concatD [prt 0 regtype])
    FType funtype -> prPrec i 0 (concatD [prt 0 funtype])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print RegType where
  prt i e = case e of
    Int -> prPrec i 0 (concatD [doc (showString "int")])
    Str -> prPrec i 0 (concatD [doc (showString "string")])
    Bool -> prPrec i 0 (concatD [doc (showString "boolean")])
    Void -> prPrec i 0 (concatD [doc (showString "void")])

instance Print FunType where
  prt i e = case e of
    Function type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])

instance Print Expr where
  prt i e = case e of
    EVar id -> prPrec i 6 (concatD [prt 0 id])
    EFun vars block -> prPrec i 6 (concatD [doc (showString "fun"), doc (showString "("), prt 0 vars, doc (showString ")"), doc (showString "{"), prt 0 block, doc (showString "}")])
    ELitInt n -> prPrec i 6 (concatD [prt 0 n])
    ELitTrue -> prPrec i 6 (concatD [doc (showString "true")])
    ELitFalse -> prPrec i 6 (concatD [doc (showString "false")])
    EApp id exprs -> prPrec i 6 (concatD [prt 0 id, doc (showString "("), prt 0 exprs, doc (showString ")")])
    EString str -> prPrec i 6 (concatD [prt 0 str])
    Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print AddOp where
  prt i e = case e of
    Plus -> prPrec i 0 (concatD [doc (showString "+")])
    Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print MulOp where
  prt i e = case e of
    Times -> prPrec i 0 (concatD [doc (showString "*")])
    Div -> prPrec i 0 (concatD [doc (showString "/")])
    Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print RelOp where
  prt i e = case e of
    LTH -> prPrec i 0 (concatD [doc (showString "<")])
    LE -> prPrec i 0 (concatD [doc (showString "<=")])
    GTH -> prPrec i 0 (concatD [doc (showString ">")])
    GE -> prPrec i 0 (concatD [doc (showString ">=")])
    EQU -> prPrec i 0 (concatD [doc (showString "==")])
    NE -> prPrec i 0 (concatD [doc (showString "!=")])


