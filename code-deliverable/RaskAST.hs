module RaskAST where 

import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map SymName RaskExpr

type SymName = String

data RaskExpr 
    = Nil                                   -- empty list, i.e. ()
    | Num       Integer                     -- an integer, e.g. 1
    | Bool      Bool                        -- a boolean, either true or false
    | UnOp      Uop RaskExpr                -- a unary operation, i.e. add1, sub1
    | BinOp     Bop RaskExpr RaskExpr       -- a binary operation, i.e. +,-,*,/
    | And       RaskExpr RaskExpr           -- the and boolean operation
    | Or        RaskExpr RaskExpr           -- the or boolean operation
    | Not       RaskExpr                    -- the not boolean operation
    | Xor       RaskExpr RaskExpr           -- the xor boolean operation
    | Comp      Cmp RaskExpr RaskExpr       -- a binary comparison, i.e. =,<,>,<=,>=
    | Sym       SymName                     -- a symbolic name, e.g. a variable, like x
    | Cond      [(RaskExpr,RaskExpr)]       -- conditional control flow
    | Else                                  -- the else block of a cond statement
    | IfElse    RaskExpr RaskExpr RaskExpr  -- if-then-else statement
    | Let       [(SymName,RaskExpr)] RaskExpr   -- let statement
    | Quote     RaskExpr                    -- quote the expression and do not evaluate it
    | List      [RaskExpr]                  -- creates a list of multiple expressions
    | Head      RaskExpr                    -- first element of a list 
    | Tail      RaskExpr                    -- all but the first element of a list
    | Map       RaskExpr RaskExpr           -- apply a function to each element of a list
    | Append    RaskExpr RaskExpr           -- append two lists
    | Filter    RaskExpr RaskExpr           -- remove all elements from a list that don't satisfy a condition
    | Reverse   RaskExpr                    -- reverse a list
    | Foldl     RaskExpr RaskExpr RaskExpr  -- compress a list by applying a function to pairs of its elements, starting with an init value
    | Cons      RaskExpr RaskExpr           -- construct a list from the two expressions, e.g. Cons x xs is like x : xs
    | Lambda    [SymName] RaskExpr          -- anonymous function with multiple arguments: e.g. Lambda ["x", "y"] (Cons y x) 
    | Apply     RaskExpr [RaskExpr]         -- apply the first expression to the second        
    | Closure   Env [SymName] RaskExpr      -- internal snapshot the environment and function definition in a closure (not directly accessible by users) 
    | RaskError String                      -- for recording errors, also not directly accessible by users
    deriving (Show, Eq)

-- a unary operation
data Uop = Add1 | Sub1
                deriving (Show, Eq, Ord)

-- a binary operation
data Bop = Plus | Minus | Times | Divide | Modulo | Max | Min | Expt
                deriving (Show, Eq, Ord)

-- a binary comparison
data Cmp = Eq | Lt | Gt | Leq | Geq
                deriving (Show, Eq, Ord)


-- eventually, we want to show the user a nicely formatted string for output
-- that is, turn an expression into a string, which is the opposite of parsing!
-- hopefully this function is straighforward to understand
unparse :: RaskExpr -> String
unparse Nil = "()"
unparse (Num n) = show n
unparse (Sym s) = s
unparse (Bool True) = "#t"
unparse (Bool False) = "#f"
unparse (And x y) = "(and " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (Or x y) = "(or " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (Not x) = "(not " ++ unparse x ++ ")"
unparse (Xor x y) = "(xor " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (UnOp Add1 x) = "(add1 " ++ unparse x ++ ")"
unparse (UnOp Sub1 x) = "(sub1 " ++ unparse x ++ ")"
unparse (BinOp Plus x y) = "(+ " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (BinOp Minus x y) = "(- " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (BinOp Times x y) = "(* " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (BinOp Divide x y) = "(/ " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (BinOp Modulo x y) = "(modulo " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (BinOp Max x y) = "(max " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (BinOp Min x y) = "(min " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (BinOp Expt x y) = "(expt " ++ unparse x ++ " " ++ unparse y ++ ")"
unparse (Cond c) = parenthesize $ "cond" : map unparseCond c
unparse (IfElse i t e) = parenthesize ["if", unparse i, unparse t, unparse e]
unparse (Let v b) = parenthesize $ ("let" : map unparseLet v) ++ [unparse b]
unparse (Quote q) = "\'" ++ unparse q
unparse (Head e) = parenthesize ["head", unparse e]
unparse (Tail e) = parenthesize ["tail", unparse e]
unparse (List l) = "\'" ++ parenthesize (map unparse l)
unparse (Map f l) = parenthesize ["map", unparse f, unparse l]
unparse (Append e1 e2) = parenthesize ["cons", unparse e1, unparse e2]
unparse (Filter f l) = parenthesize ["filter", unparse f, unparse l]
unparse (Reverse e) = parenthesize ["reverse", unparse e]
unparse (Foldl f i l) = parenthesize ["foldl", unparse f, unparse i, unparse l]
unparse (Cons e1 e2) = parenthesize ["cons", unparse e1, unparse e2]
unparse (Lambda params body) = parenthesize $ ["lambda"] ++ params ++ [unparse body]
unparse (Apply expr args) = parenthesize $ [unparse expr] ++ map unparse args 

-- internal errors can get reported to users, even though they do not have internal access to them
unparse (RaskError e) = "RASKELL ERROR: " ++ e

-- closures are not visible to users, it is an internal implementation detail
unparse (Closure _ _ _) = error "attempt to print out internal closure" 

-- helper function for unparsing cond expressions
unparseCond :: (RaskExpr,RaskExpr) -> String
unparseCond (x,y) = parenthesize [unparse x, unparse y]

-- helper function for unparsing let expressions
unparseLet :: (SymName,RaskExpr) -> String
unparseLet (x,y) = parenthesize [x, unparse y]

-- wrap a list of 
parenthesize :: [String] -> String
parenthesize exprs = "(" ++ unwords exprs ++ ")"
    
