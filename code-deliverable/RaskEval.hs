module RaskEval where 

import RaskAST 
import RaskParser

import Data.Map (Map)
import qualified Data.Map as Map

-- ******************************
-- ENVIRONMENT HANDLING
-- ******************************

-- inserting a symbol and value into the environment is just a Map.insert
envInsert :: SymName -> RaskExpr -> Map SymName RaskExpr -> Map SymName RaskExpr
envInsert k v m = Map.insert k v m 

-- inserting several symbols and values into the environment is done
-- by recursively inserting one pair of symbol and value at a time
-- (note: this could be done with a fold as well)
envInsertMany :: [SymName] -> [RaskExpr] -> Map SymName RaskExpr -> Map SymName RaskExpr
envInsertMany [] [] m = m
envInsertMany (k:ks) (v:vs) m = envInsert k v (envInsertMany ks vs m) 
envInsertMany _ (v:vs) _ = error "arity mismatch: more args than params, should have been caught at interpretation time"
envInsertMany (k:ks) _ _ = error "arity mismatch: more params than args, should have been caught at interpretation time"

-- looking up a symbol is just a Map lookup, but if the symbol is not
-- found, we return an error that there is an unbound identifier
envLookup :: SymName -> Env -> RaskExpr
envLookup k m = Map.findWithDefault (RaskError $ "unbound identifier: " ++ k) k m

-- the initial environment is empty
initEnv :: Env
initEnv = Map.fromList []


-- ******************************
-- RUNNING AN EXPRESSION
-- ******************************

-- to run an expression, evaluate it in the initial environment
run :: RaskExpr -> RaskExpr
run expr = eval initEnv expr


-- ******************************
-- EVALUATION
-- ******************************

-- eval takes in an environment and an expression,
-- evalautated the expression in that environment,
-- and produces the resulting expression
eval :: Env -> RaskExpr -> RaskExpr

-- the expression () is an empty application
eval _ Nil = RaskError "empty application: RaskExprected procedure applied to 0 or more arguments"

-- a number evaluates to itself
eval _ (Num x) = Num x

-- a boolean evaluates to itself

eval _ (Bool b) = Bool b

-- an else expression evaluates to itself
eval _ Else = Else

-- to evaluate a symbol, look it up in the environment 
eval env (Sym v) = envLookup v env

-- a closure evaluates to itself
eval _ c@(Closure env v expr) = c

-- an error evaluates to itself
eval _ re@(RaskError e) = re

-- a quoted expression evaluates to itself
eval env (Quote q) = (Quote q) 

-- a unary operation applies that operation to the evaluated expression
eval env (UnOp uop expr) = evalUnOp uop (eval env expr)

-- a binary operation applies that operation to the evaluated subexpressions 
eval env (BinOp bop left right) = evalBinOp bop (eval env left) (eval env right)

-- a comparison compares the evaluated subexpressions 
eval env (Comp cmp left right) = evalComp cmp (eval env left) (eval env right)

-- boolean operations evaluate to that operation to the evaluated subexpressions
eval env (And (Bool l) (Bool r)) = Bool (l && r)
eval env (And left right) = eval env (And (eval env left) (eval env right))

eval env (Or (Bool l) (Bool r)) = Bool (l || r)
eval env (Or left right) = eval env (Or (eval env left) (eval env right))

eval env (Not (Bool e)) = Bool (not e)
eval env (Not expr) = eval env (Not (eval env expr))

eval env (Xor (Bool l) (Bool r)) = Bool ((l && not r) || (not l && r))
eval env (Xor left right) = eval env (Xor (eval env left) (eval env right))

-- conditional control flow evaluates the first true block
eval env (Cond []) = Nil
eval env (Cond ((hif,hthen):t)) = if (hif == Else) || (eval env hif == Bool True) 
                                then eval env hthen
                                else eval env (Cond t)

-- if-then-else control flow evaluates either the then or else block based on if the if block is true
eval env (IfElse i t e) = if eval env i == Bool True
                          then eval env t
                          else eval env e

-- a let expression adds new variables to the environment, then evaluates the body
eval env (Let [] body) = eval env body
eval env (Let dec@((hsym,hval):t) body) = eval (envInsert hsym (eval env hval) env) (Let t body)

-- a cons expression evaluates to a cons of the evaluated subexpressions
eval env (Cons e1 e2) = Cons (eval env e1) (eval env e2)

-- a list expression evaluates to a list made of all the evaluated subexpression
eval env (List []) = Quote Nil
eval env (List (h:t)) = Cons (eval env h) (eval env (List t))

-- the head of a cons is the first expression, evaluated
eval env (Head (Cons e1 e2)) = eval env e1

-- if the first expression is not already a Cons, evaluate it (hopefully returning a Cons!)
eval env (Head expr) = eval env (Head (eval env expr))

-- the tail of a cons is the second expression, evaluated
eval env (Tail (Cons e1 e2)) = eval env e2

-- if the second expression is not already a Cons, evaluate it (hopefully returning a Cons!)
eval env (Tail expr) = eval env (Tail (eval env expr))

-- a map expression evaluates a function on each element of a list
eval env (Map _ (Quote Nil)) = Quote Nil
eval env (Map func (Cons h t)) = eval env (Cons (eval env (Apply (eval env func) [eval env h])) (eval env (Map func t)))
eval env (Map func expr) = eval env (Map func (eval env expr))

-- an append expression evaluates to a list formed by appending two lists
eval env (Append l1 (Quote Nil)) = eval env l1
eval env (Append (Quote Nil) l2) = eval env l2
eval env (Append (Cons h1 t1) l2@(Cons _ _)) = eval env (Cons h1 (eval env (Append t1 (eval env l2))))
eval env (Append l1 l2) = eval env (Append (eval env l1) (eval env l2))

-- filter the contents a list by a boolean function
eval env (Filter func (Quote Nil)) = Quote Nil
eval env (Filter func (Cons h t)) = if eval env (Apply (eval env func) [eval env h]) == Bool True
                                    then eval env (Cons h (eval env (Filter func (eval env t))))
                                    else eval env (Filter func t)
eval env (Filter func expr) = eval env (Filter func (eval env expr))

-- reverse a list
eval env (Reverse (Quote Nil)) = Quote Nil
eval env (Reverse (Cons h t)) = eval env (Append (eval env (Reverse t)) (eval env (List [eval env h])))
eval env (Reverse expr) = eval env (Reverse (eval env expr))

-- fold a list into a single value using a function and an init value
eval env (Foldl _ init (Quote Nil)) = eval env init
eval env (Foldl func init (Cons h t)) = eval env (Foldl (eval env func) (eval env (Apply (eval env func) [eval env init, eval env h])) (eval env t))
eval env (Foldl func init expr) = eval env (Foldl (eval env func) (eval env init) (eval env expr))

-- an anonymous function evaluates to a closure (snapshot the function body and params and the envirnonment)
eval env (Lambda params body) = Closure env params body

-- to evaluate an application, eval the function (which should eval 
-- to a Closure) then evaluate the arguments (call by value), 
-- then insert them into the environment along with the associated 
-- symbols (parameters) and evaluate the body in the new environment
eval env (Apply fexpr expr) = 
    let (Closure closureEnv syms body) = eval env fexpr 
        argvals = map (eval env) expr 
        env' = (envInsertMany syms argvals closureEnv)
        in
            eval env' body

-- helper function for unary operations
evalUnOp :: Uop -> RaskExpr -> RaskExpr
evalUnOp Add1 (Num x) = Num (x + 1)
evalUnOp Sub1 (Num x) = Num (x - 1)

-- helper function for binary operations
evalBinOp :: Bop -> RaskExpr -> RaskExpr -> RaskExpr
evalBinOp Plus (Num x) (Num y) = Num (x + y)
evalBinOp Minus (Num x) (Num y) = Num (x - y)
evalBinOp Times (Num x) (Num y) = Num (x * y)
evalBinOp Divide (Num x) (Num y) = Num (x `div` y)
evalBinOp Modulo (Num x) (Num y) = Num (x `mod` y)
evalBinOp Max (Num x) (Num y) = Num (max x y)
evalBinOp Min (Num x) (Num y) = Num (min x y)
evalBinOp Expt (Num x) (Num y) = Num (x ^ y)

-- helper function for comparisons
evalComp :: Cmp -> RaskExpr -> RaskExpr -> RaskExpr
evalComp Eq (Num x) (Num y) = Bool (x == y)
evalComp Lt (Num x) (Num y) = Bool (x < y)
evalComp Gt (Num x) (Num y) = Bool (x > y)
evalComp Leq (Num x) (Num y) = Bool (x <= y)
evalComp Geq (Num x) (Num y) = Bool (x >= y)



