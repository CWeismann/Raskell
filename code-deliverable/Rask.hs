module Main where 

import RaskAST 
import RaskParser
import RaskEval
import System.IO
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map


-- TO COMPILE at the docker command line: 
--        ghc -o raskell Rask.hs

-- TO RUN THE REPL at the docker command line:
--        ./rask
--        #^^ this is a wrapper script for rlwrap ./raskell 

-- the main function is just the raskell REPL
main = raskell

-- this is the main loop: read input, parse, eval, print result, repeat
raskell :: IO ()
raskell = do
  input <- replRead
  unless (input == ":quit" || input == ":q")
       $  replPrint (replEval input)
       >> raskell -- this is the "loop" part of the read, eval, print, loop (REPL)

-- display a prompt and read user input
replRead :: IO String
replRead =  putStr "raskell> "
     >> hFlush stdout
     >> getLine

-- eval here is actually: parse, run, unparse result
replEval :: String -> String
replEval = unparse . run . (parse expr)

-- print is just "put string with new line"
replPrint :: String -> IO ()
replPrint = putStrLn