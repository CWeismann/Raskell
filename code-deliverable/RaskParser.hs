module RaskParser (module RaskParser, parseFile, parse) where

import RaskAST 
import ParserCombinators
import Data.Char

-- an atom is just a symbol
atom :: Parser RaskExpr 
atom =  (symbol             >>=: Sym)

-- a symbol is a non-keyword identifier
symbol :: Parser SymName
symbol = identifier <=> (`notElem` reservedWords)

-- reserved words
reservedWords :: [String]
reservedWords = [ "head", "cons", "lambda", "quote", "add1", "sub1", "modulo", "min","max",
                  "expt", "and", "or", "not", "xor", "cond","if", "else","let","list","tail",
                  "map","append","filter","reverse","foldl"]

-- the nil
nil :: Parser RaskExpr 
nil = text "()" >>: Nil

-- booleans
bool :: Parser RaskExpr
bool =  (text "#t" >>: Bool True)
    <|> (text "#f" >>: Bool False)


-- numbers
numExpr :: Parser RaskExpr
numExpr = number >>=: Num

-- unary operations, add1, sub1, consist of the operation
-- name followed by a numeric expression
unOpExpr :: Parser RaskExpr
unOpExpr = parens (text "add1" <-+> (whitespace <-+> expr) >>=: UnOp Add1)
       <|> parens (text "sub1" <-+> (whitespace <-+> expr) >>=: UnOp Sub1)

-- binary operations, +, -, *, /, consist of the operation
-- name followed by two numeric expressions
binOpExpr :: Parser RaskExpr
binOpExpr = parens (sym '+' <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (BinOp Plus))
        <|> parens (sym '-' <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (BinOp Minus))
        <|> parens (sym '*' <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (BinOp Times))
        <|> parens (sym '/' <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (BinOp Divide))
        <|> parens (text "modulo" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (BinOp Modulo))
        <|> parens (text "max" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (BinOp Max))
        <|> parens (text "min" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (BinOp Min))
        <|> parens (text "expt" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (BinOp Expt))

-- comparisons, =, <, >, <=, >=, consist of the operation
-- name followed by two numeric expressions
compExpr :: Parser RaskExpr
compExpr = parens (text "=" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (Comp Eq))
        <|> parens (text "<" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (Comp Lt))
        <|> parens (text ">" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (Comp Gt))
        <|> parens (text "<=" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (Comp Leq))
        <|> parens (text ">=" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr >>=: uncurry (Comp Geq))

-- boolean operations, and, or, not, xor, consist of the operation
-- name followed by two boolean expressions
andExpr :: Parser RaskExpr
andExpr = parens (text "and" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr) >>=: uncurry And

orExpr :: Parser RaskExpr
orExpr = parens (text "or" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr) >>=: uncurry Or

notExpr :: Parser RaskExpr
notExpr = parens (text "not" <-+> (whitespace <-+> expr)) >>=: Not

xorExpr :: Parser RaskExpr
xorExpr = parens (text "xor" <-+> (whitespace <-+> expr <+-> whitespace) <+> expr) >>=: uncurry Xor

-- cond expressions consist of the keyword `cond`, then a list of test-body pairs
condExpr :: Parser RaskExpr
condExpr = parens $ text "cond" <-+> many (parens ((expr <+-> whitespace) <+> expr)) >>=: Cond

-- an else statement consists of the keyword `else`
elseExpr :: Parser RaskExpr
elseExpr = parens (text "else" >>: Else)
       <|> (text "else" >>: Else)

-- if-then-else expressions consist of the keyword `if`, a test block, a then block, and
-- an else block.
ifExpr :: Parser RaskExpr
ifExpr = parens $ text "if" <-+> (whitespace <-+> expr) <+> (whitespace <-+> expr) <+> (whitespace <-+> expr) >>=: uncurry (uncurry IfElse)

-- let expressions consist of the keyword `let`, a list of variables and their
-- values, and the expression to evaluate
letExpr :: Parser RaskExpr
letExpr = parens $ text "let" <-+> parens (many (parens ((symbol <+-> whitespace) <+> expr))) <+> (whitespace <-+> expr) >>=: uncurry Let

-- a quoted expression either uses the keyword `quote` or uses
-- a single quote (apostrophe) before the quoted expression
quoteExpr :: Parser RaskExpr 
quoteExpr =      (parens $ text "quote" <-+> expr    >>=: Quote)
            <|>  (text "\'" <-+> expr                >>=: Quote)

-- a lambda expression is parenthesized. it starts with the keyword `lambda`,
-- is followed by a parenthesized list of input parameters (symbols),
-- and then an expression (the body of the lambda function)
lambdaExpr :: Parser RaskExpr 
lambdaExpr = parens $ text "lambda" <-+> parens (many symbol) <+> expr >>=: uncurry Lambda

makeListExpr :: Parser RaskExpr
makeListExpr = parens $ text "list" <-+> many expr >>=: List

-- a head expression is parenthesized. it starts with the keyword `head`
-- and is followed by the expression we wish to take the head of 
headExpr :: Parser RaskExpr 
headExpr = parens $ text "head" <-+> expr >>=: Head

-- a tail expression is parenthesized. it starts with the keyword `tail`
-- and is followed by the expression we wish to take the tail of 
tailExpr :: Parser RaskExpr 
tailExpr = parens $ text "tail" <-+> expr >>=: Tail

-- a map expression is parenthesized. it starts with the keyword `map` and
-- is followed by a function and the list we want to map the function onto
mapExpr :: Parser RaskExpr
mapExpr = parens $ text "map" <-+> (expr <+> (whitespace <-+> expr)) >>=: uncurry Map

-- an append expression is parenthesized. it starts with the keyword `append`
-- and is followed by the two lists that we want to append together
appendExpr :: Parser RaskExpr
appendExpr = parens $ text "append" <-+> (expr <+> (whitespace <-+> expr)) >>=: uncurry Append

-- a map expression is parenthesized. it starts with the keyword `map` and
-- is followed by a function to filter by and the list we want to filter
filterExpr :: Parser RaskExpr
filterExpr = parens $ text "filter" <-+> (expr <+> (whitespace <-+> expr)) >>=: uncurry Filter

-- a reverse expression is parenthesized. it starts with the keyword `reverse`
-- and is followed by the expression we wish to reverse
reverseExpr :: Parser RaskExpr 
reverseExpr = parens $ text "reverse" <-+> expr >>=: Reverse

-- a reverse expression is parenthesized. it starts with the keyword `foldl`
-- a function, an initial value, and a list to fold with the function
foldlExpr :: Parser RaskExpr
foldlExpr = parens $ text "foldl" <-+> (whitespace <-+> expr) <+> (whitespace <-+> expr) <+> (whitespace <-+> expr) >>=: uncurry (uncurry Foldl)

-- a cons expression is parenthesized. it starts with the keyword `cons`
-- and is followed by the two expressions that we want to cons together  
consExpr :: Parser RaskExpr 
consExpr = parens $  text "cons" <-+> expr <+> expr >>=: uncurry Cons

-- a function / procedure application expression is parenthesized. 
-- it starts with an expression (the function / procedure being called)
-- and is followed by zero or more expressions (the arguments to the function call)
applyExpr :: Parser RaskExpr 
applyExpr = parens $ expr <+> many expr >>=: uncurry Apply

-- a list expression is any one of the expressions that are represented
-- by parenthesized lists 
listExpr :: Parser RaskExpr
listExpr =      makeListExpr
            <|> headExpr 
            <|> tailExpr
            <|> mapExpr
            <|> appendExpr
            <|> filterExpr
            <|> reverseExpr
            <|> foldlExpr
            <|> consExpr 
            <|> quoteExpr 
            <|> lambdaExpr 
            <|> applyExpr 
            <|> nil

boolExpr :: Parser RaskExpr
boolExpr =      bool
            <|> andExpr
            <|> orExpr
            <|> notExpr
            <|> xorExpr

-- an expression is either an atom or a list expression
expr :: Parser RaskExpr 
expr = elseExpr <|> atom <|> numExpr <|> unOpExpr <|> binOpExpr <|> boolExpr <|> compExpr
   <|> condExpr <|> ifExpr <|> letExpr <|> listExpr