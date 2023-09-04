{-|
Module       : ParserCombinators
Description  : Re-definitions of basic parsers and parser combinators from
               class and lab . Also, some definitions of more parser combinators.               
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

-- The following complicated module declaration makes it so that we can use our
-- parser-combinator library by writing the line:
--
--   import ParserCombinators
--
-- (Without this complicated declaration, we'd have to re-import the basic
-- parsers and parser combinators that are defined elsewhere.)
module ParserCombinators (

      -- * Types
      Parser

      -- * Primitive parsers and combinators
      -- | The types of these items are a little strange because we implement them
      --   using a more general feature of Haskell (Alternatives and Monads).
      --   If you want to learn more about the types and purposes of these items, 
      --   __see the documentation for basic parsers on the course website.__
    , get
    , return
      -- | A parser that always succeeds and returns the specified value. Think of the type as:
      --
      -- >  return :: a -> Parser a
    , pfail
    , fail
      -- | A parser that always fails with the given message. Think of the type as:
      --
      -- >  return :: String -> Parser a
    , (<|>)
      -- | Alternatives. Succeeds if either parser succeeds.  Think of the type as:
      --
      -- >  (<|>) :: Parser a -> Parser a -> Parser a
    , (>>=)
      -- | The "and then" (or "bind") operator. Think of the type as:
      --
      -- >  (>>=) :: Parser a -> (a -> Parser b) -> Parser b

      -- * Parsing functions
    , parse, parseFile

      -- * Useful combinators

      -- ** Variations on '>>='
    , (>>=:), (>>:)

      -- ** Combining multiple parsers and parse results
    , (<+>), (<+->), (<-+>), (<-+->), (<:>), (<++>)

      -- ** Filtering
    , (<=>)

      -- * Single characters
    , getCharThat, digit, letter, space, alphanum, char

      -- * Strings
    , text, string, litstring, stringchar

      -- * Symbols
    , sym, openparen, closeparen, parens, openbrace, closebrace, braces

      -- * Whitespace
    , whitespace, skipws

      -- * Identifiers
    , identifier, ident

      -- * Numbers
    , number, num, double

      -- * Repetition, alternation, options, and delimiting
    , many
      -- | Zero or more matches. Think of the type as:
      -- 
      -- > many :: Parser a -> Parser [a]
    , some
      -- | One or more matches. Think of the type as:
      -- 
      -- > some :: Parser a -> Parser [a]
    , many1, skipMany, skipMany1, optional, perhaps, manyEndingWith
    , someEndingWith, between, sepBy, sepBy1, endBy, endBy1, chainr1, chainl1

      -- * Error messages
    , (<??>), (<???>)

    ) where

import Data.Char as Char  -- so we can use toUpper, isDigit, etc. in this file.

--------------------------------------------------------------------------------
-- Basic parsers and parser combinators 
--------------------------------------------------------------------------------

import ParserBase

-- The import above gives us:
--
--    pfail  :: Parser a
--    return :: a -> Parser a
--    get    :: Parser Char
--    <|>    :: Parser a -> Parser a -> Parser a 
--    <||>   :: Parser a -> Parser a -> Parser a 
--    >>=    :: Parser a ->  (a -> Parser b) -> Parser b
--
-- and
--
--    parse  :: String -> Parser a -> a
--
-- The course web page has more information about what these things do, in
-- particular, you can check out
--
--     https://www.cs.hmc.edu/cs131/ParserBaseOperations


--------------------------------------------------------------------------------
-- Useful parser combinators
--
-- You *don't* need to recall the internals of how they work.  In fact,
-- >> YOU DO NOT NEED TO BE ABLE TO MAKE SENSE OF THIS IMPLEMENTATION <<
--
-- Note that the functions below are written in a quite different style from
-- the style you probably saw in class, because they use library functions
-- (and function composition, a.k.a. point free style).  Also, some of
-- the types are more general than the versions from class.
--------------------------------------------------------------------------------

-- | Given a parser, transform its result by passing it through a provided function
-- 
--   Think of the type as
-- 
--   > (>>=:) :: Parser a -> (a -> b) -> Parser b
infixl 1 >>=:
(>>=:) :: Functor parser => parser a -> (a -> b) -> parser b
(>>=:) = flip fmap


-- | Given a parser, ignore is result and instead, if the parser succeeds
--   always return a specific value.
-- 
--   Think of the type as
-- 
--   > (>>:) :: Parser a -> b -> Parser b
infixl 1 >>:
(>>:) :: Functor parser => parser a -> b -> parser b
p >>: v  = fmap (const v) p


-- | Given a parser and a predicate, return the result of the parser only if
--   it also satisfies the predicate.
-- 
--   Think of the type as
-- 
--   > (<=>) :: Parser a -> (a -> Bool) -> Parser a
infix 7 <=> 
(<=>) :: MonadPlus parser => parser a -> (a -> Bool) -> parser a
(<=>) = flip mfilter


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input
--     2. runs the second parser on what remains in the input
--     3. returns a pair of their results
--   If either of the parsers fail, the whole thing fails.
-- 
--   Think of the type as
-- 
--   > (<+>) :: Parser a -> Parser b -> Parser (a, b)
infixl 6 <+>
(<+>) :: Applicative parser => parser a -> parser b -> parser (a,b)
p <+> q = (,) <$> p <*> q
    -- If you think it the code is confusing, it could be worse! We could have
    --  written it in point-free style as:
    --      (<+>) = (((,) <$>) .) . (<*>)


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input to generate a thing
--     2. runs the second parser on what remains in the input to make a list of
--        the same kind of thing
--     3. returns a list of things
--   If either of the parsers fail, the whole thing fails.
-- 
--   Think of the type as
-- 
--   > (<:>) :: Parser a -> Parser [a] -> Parser [a]
infixr 5 <:>
(<:>) :: Applicative parser => parser a -> parser [a] -> parser [a]
p <:> q = (:) <$> p <*> q


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input
--     2. runs the second parser on what remains in the input
--     3. returns ONLY the results of the first one.
--
-- For example:
--     *ParserCombinators> parse (get <+-> get) "ab"
--     'a'    
-- 
--   Think of the type as
-- 
--   > (<+->) :: Parser a -> Parser b -> Parser a
infixl 6 <+->
(<+->) :: Applicative parser => parser a -> parser b -> parser a
(<+->) = (<*)


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input
--     2. runs the second parser on what remains in the input
--     3. returns ONLY the results of the second one.
--
-- For example:
--     *ParserCombinators> parse (get <-+> get) "ab"
--     'b'    
-- 
--   Think of the type as
-- 
--   > (<-+>) :: Parser a -> Parser b -> Parser b
infixl 6 <-+>
(<-+>) :: Applicative parser => parser a -> parser b -> parser b
(<-+>) = (*>)


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input
--     2. runs the second parser on what remains in the input
--     3. ignores both results and returns a Haskell unit, ---[i.e., this]--> ()
--
-- For example:
--     *ParserCombinators> parse (get <-+-> get) "ab"
--     ()
-- 
--   Think of the type as
-- 
--   > (<-+->) :: Parser a -> Parser b -> Parser ()
infixl 6 <-+->
(<-+->) :: Applicative parser => parser a -> parser b -> parser ()
p <-+-> q = p *> q *> pure ()


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input to generate a list
--     2. runs the second parser on what remains in the input to generate another
--        list
--     3. concatenate the two lists
--   If either of the parsers fail, the whole thing fails.
-- 
--   Think of the type as
-- 
--   > (<++>) :: Parser [a] -> Parser [a] -> Parser [a]
infixr 5 <++>
(<++>) :: Applicative parser => parser [a] -> parser [a] -> parser [a]
p <++> q = (++) <$> p <*> q


--------------------------------------------------------------------------------
-- Parsers for single characters
--------------------------------------------------------------------------------


-- | Return a character result only if the character satisfies a given predicate
getCharThat :: (Char -> Bool) -> Parser Char
getCharThat predicate = get <=> predicate
                      <??> "Different kind of character expected"


-- | Parse a single character that is a digit                   
digit :: Parser Char
digit  = getCharThat isDigit


-- | Parse a single alphabetic character (uppercase or lowercase)
letter :: Parser Char
letter = getCharThat isAlpha


-- | Parse a character that is whitespace (e.g., space, tab, newline, etc.)
space :: Parser Char
space = getCharThat isSpace


-- | Parse a character that is either a digit or a letter
alphanum :: Parser Char
alphanum = digit <|> letter


-- | Returns c, if c is the next character in the input
--   NOTE: This parser adds a better error message using <??>. 
--         To see the difference, compare the error messages from these two 
--         parsers that look for a single 'x':
--                parse (get <=> (== 'x')) "y"
--                parse (char 'x') "y"
char :: Char -> Parser Char
char c = getCharThat (==c)
         <??> "Expected '" ++ [c] ++ "'"


--------------------------------------------------------------------------------
-- Parsers for strings
--------------------------------------------------------------------------------


-- | Like 'string' but skips any whitespace that precedes the string.
text :: String -> Parser String
text str = skipws (string str)
           <??> "\"" ++ str ++ "\" expected"


-- | Returns a specific sequence of characters, if those are exactly the next
--   characters in the input
string :: String -> Parser String
string ""         = return ""
string str@(h:hs) = char h <:> string hs
                    <??> "Expected '" ++ str ++ "'"


-- | A string literal is a sequence of string characters inside quotation
--   marks, optionally surrounded by some whitespace.
litstring :: Parser String
litstring = skipws (char '"' <-+> many stringchar <+-> char '"')


-- | A string character is either any character that is not a double quote,
--   or the escape sequence \" (which we interpret as a double quote
--   character inside our string)
stringchar :: Parser Char
stringchar =   getCharThat (/= '"')
           <|> (string "\\\""                       >>: '\"')                    


--------------------------------------------------------------------------------
-- Parsers for symbols
--------------------------------------------------------------------------------


-- | Like 'char' but skips any whitespace that precedes the char.
sym :: Char -> Parser Char
sym ch  = skipws (char ch)
           <??> "'" ++ [ch] ++ "' expected"


-- | Parses the character '('
openparen :: Parser Char
openparen  = sym '('


-- | Parses the character ')'
closeparen :: Parser Char
closeparen = sym ')'


-- | Given a parser p, succeeds if the input string contains a
--   parenthesis-delimited string that matches the parser p.
parens :: Parser a -> Parser a
parens = between openparen closeparen


-- | Parses the character '{'
openbrace :: Parser Char
openbrace  = sym '{'


-- | Parses the character '}'
closebrace :: Parser Char
closebrace = sym '}'


-- | Given a parser p, succeeds if the input string contains a
--   braces-delimited string that matches the parser p.
braces :: Parser a -> Parser a
braces = between openbrace closebrace


--------------------------------------------------------------------------------
-- Parsers for whitespace
--------------------------------------------------------------------------------


-- | A parser that ignores whitespace (by consuming it from the input and
--   returning unit).
whitespace :: Parser ()
whitespace = skipMany space


-- | Returns a parser that parses what p does, but skips any whitespace that
--   appears at the beginning of the input string.
skipws :: Parser a -> Parser a
skipws p = whitespace <-+> p


--------------------------------------------------------------------------------
-- Parsers for identifiers and reserved words
--------------------------------------------------------------------------------


-- | Parse an identifier, defined here as a letter, followed by zero or more
--   alphanumeric characters.
ident :: Parser String
ident =      (letter <+> many alphanum          >>=: \(l,ls) -> l:ls)
             <??> "<ident> expected"


-- | Like 'ident' but skips any whitespace that precedes the identifier.
identifier :: Parser String
identifier = skipws ident


--------------------------------------------------------------------------------
-- Parsers for numbers
--------------------------------------------------------------------------------


-- | Parse an Integer
num :: Parser Integer
num =     (optional (char '-') <+> some digit    >>=: \(m,d) -> read (m++d))
        <??> "<num> expected"


-- | Like 'num' but skips any whitespace that precedes the number.
number :: Parser Integer
number = skipws num

-- | Parse a double, including scientific notation
doub :: Parser Double
doub =  (integer <+> decimal  >>=: \(i, d) -> (read :: String -> Double) (i ++ d))
    <|> (integer              >>=: (read :: String -> Double))
    where numeric :: Parser String
          numeric = some digit

          integer :: Parser String
          integer = (char '-' <:> numeric) <|> numeric

          decimal :: Parser String
          decimal =  char '.' <:> (
                        -- It has an exponent
                        (numeric <+> exponent >>=: uncurry (++))
                        -- Just a decimal, nothing else                        
                 <|> numeric)

          exponent :: Parser String
          exponent = char 'e' <:> (integer <|> (char '+' <:> numeric))

-- | Like 'doub' but skips any whitespace that precedes the number.
double :: Parser Double
double = skipws doub

--------------------------------------------------------------------------------
-- Repetition, alternation, options, and delimiting
--------------------------------------------------------------------------------


-- | Equivalent to 'some'
--   (because some people like to call the "some" parser "many1")
many1 ::  Parser a -> Parser [a]
many1 = some


-- | Like 'many' but instead of returning the results, throws them away.
--   Think of the type as:
--
--   > skipMany :: Parser a -> Parser ()
skipMany :: Alternative f => f a -> f ()
skipMany p  =     (p <-+-> skipMany p)
              <|> pure ()


-- | Like 'many1' but instead of returning the results, throws them away.
--   Think of the type as:
--
--   > skipMany1 :: Parser a -> Parser ()
skipMany1 :: Alternative parser => parser a -> parser ()
skipMany1 p = p <-+-> skipMany p


-- | Tries to parse a p, but also succeeds if it doesn't find a p
--   You can think of the type of optional as being either of these:
--
--   > optional :: Parser a -> Parser (Maybe a)
--   > optional :: Parser a -> Parser [a]
--
--   but actually we use a more general type .
optional :: (Alternative parser, Alternative t) => parser a -> parser (t a)
optional p = (pure <$> p) <|> pure empty


-- | Like optional p, but has different type, it assumes we're trying
--   to parse something type that has a built-in notion of emptiness (e.g.,
--   strings with "", lists with [], etc.), specifically something with mzero
--   value.  If we can't parse the thing, we return that 'empty' value.
--   You can think of the type of perhaps as being one of these:
--
--   > perhaps :: Parser String -> Parser String
--   > perhaps :: Parser [a] -> Parser [a]
--   > perhaps :: Parser (Maybe a) -> Parser (Maybe a)
--
--   but actually we use a more general type.
perhaps :: (Alternative p, Monad m, Alternative m) => p (m a) -> p (m a)
perhaps = (join <$>) . optional


-- |  Equivalent to
--       many p <+-> end
--    *except* that it the above might give error messages related to
--    not being able to parse end (because many always succeeds), whereas this
--    version can give the best error message out of the one for not parsing p
--    and not parsing end.
--
--    In practice, our strategy of choosing the deepest error message should
--    mean that we don't need this function.
--
--   Think of the type as:
--
--   > manyEndingWith :: Parser a -> Parser b -> Parser [b]
manyEndingWith :: Alternative parser => parser b -> parser a -> parser [a]
manyEndingWith end p =     (end                        >>:  [])
                       <|> (p <+> manyEndingWith end p >>=: \(r,rs) -> r:rs)
                    

-- | Equivalent to
--      some p <+-> end
--   *except* that it the above might give error messages related to
--   not being able to parse end (because some always succeeds if it can read
--   at least one p), whereas this version can give the best error message out
--   of the one for not parsing p and not parsing end.
--
--   In practice, our strategy of choosing the deepest error message should
--   mean that we don't need this function.
--
--   Think of the type as:
--
--   > someEndingWith :: Parser a -> Parser b -> Parser [b]
someEndingWith :: Alternative parser => parser b -> parser a -> parser [a]
someEndingWith end p = (:) <$> p <*> manyEndingWith end p


-- | Parse something that is surrounded by delimiters (e.g., parentheses).
--   Note the order of its arguments: it takes the parser's delimiters *first*,
--   and *then* the thing to parse inside them
--
--   Think of the type as:
--
--   > between :: Parser a -> Parser b -> Parser c -> Parser c
between :: Applicative parser =>
               parser open -> parser close -> parser a -> parser a
between open close p = open <-+> p <+-> close     


-- | Given two parsers p and sep, succeeds if the input string contains
--   a sep-delimited sequence of one or more things that match p. The delimiters
--   will be thrown away and we'll be left with a list of all the matches for p.        
--
--   Think of the type as:
--
--   > sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 :: Alternative parser => parser a -> parser sep -> parser [a]
sepBy1 p sep = p <:> many (sep <-+> p)


-- | Given two parsers p and sep, succeeds if the input string contains
--   a sep-delimited sequence of zero or more things that match p. The delimiters
--   will be thrown away and we'll be left with a (possibly empty) list of all
--   the matches for p. 
--
--   Think of the type as:
--
--   > sepBy :: Parser a -> Parser b -> Parser [a]
sepBy :: Alternative parser => parser a -> parser sep -> parser [a]
sepBy p sep =    sepBy1 p sep
             <|> pure []


-- | Similar to 'sepBy', but the delimiter must also appear at the end.             
--
--   Think of the type as:
--
--   > endBy :: Parser a -> Parser b -> Parser [a]
endBy :: Alternative parser => parser a -> parser sep -> parser [a]
endBy p sep = many (p <+-> sep)


-- | Similar to 'sepBy1', but the delimiter must also appear at the end.             
--
--   Think of the type as:
--
--   > endBy1 :: Parser a -> Parser b -> Parser [a]
endBy1 :: Alternative parser => parser a -> parser sep -> parser [a]
endBy1 p sep = some (p <+-> sep)


-- | Adapts 'foldr' to work on parse results
--
--   Think of the type as:
--
--   > chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 :: Alternative parser => parser a -> parser (a -> a -> a) -> parser a
chainr1 p op = flip (foldr (\(r,f) a -> f r a)) <$> many (p <+> op) <*> p 


-- | Non-greedy chainr1 
--   The above definition of chainr1 is greedy, if it sees "1 + 2 + 3 + !", 
--   it'll fail the whole parse at the "!" rather than successfully parsing
--   "1 + 2 + 3" and leaving the "+ !" unread.  That's usually a good thing
--   because it gives better error messages, but sometimes in a more ambigious
--   grammar it'll cause problems.  This version gives that latter behavior.
chainr1weak :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1weak p op = 
    do first <- p
       succeeding first (do comb  <- op
                            rest  <- chainr1weak p op
                            return (comb first rest))


-- | Adapts 'foldl' to work on parse results
--
--   Think of the type as:
--
--   > chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 :: Alternative parser => parser a -> parser (a -> a -> a) -> parser a
chainl1 p op = foldl (\a (f,r) -> f a r) <$> p <*> many (op <+> p)


--------------------------------------------------------------------------------
-- Better error messages 
-- 
-- These combinators allow us to have nicer error messages when parsing fails.
-- You don't need to modify these combinators.
--------------------------------------------------------------------------------


-- | Given a parser p, makes a new parser where if p fails, the new parser
--   also fails, but unconditionally replaces p's error message with
--   errMsg. All error information from p (including how far it got) is thrown
--   away. (This operator uses '<||>' which is identical to '<|>' except that it
--   handles error messages slightly differently).
infixl 3 <??>
(<??>) :: Parser a -> String -> Parser a
parser <??> message = parser <||> fail message


-- | Given a parser p, makes a new parser where if p fails, the new parser
--   also fails, but it can replace a parser's failure error message with
--   a new one.  But unlike '<??>', we only do the replacement if the parser got
--   *nowhere* with things. If it made  some headway at all, we let its error
--   message stand, in the hope it'll be more useful.
infixl 3 <???>
(<???>) :: Parser a -> String -> Parser a
parser <???> message = parser <|> fail message

