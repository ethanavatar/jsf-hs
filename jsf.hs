module Main where

import System.Environment
import System.IO
import Data.Maybe
import Data.List
import Data.Char

zero :: String
zero = "+[]"

one :: String
one = "+!![]"

number :: Int -> String
number i = case i of
    0 -> zero
    _ -> [] ++ (iterate (++ one) "" !! i)

string :: [Char] -> [Char]
string cs = concat (intersperse "+" (map char cs))

char :: Char -> String
char 'a' = "(+{}+[])[" ++ (number 1) ++ "]"
char 'b' = "({}+[])[" ++ (number 2) ++ "]"
char 'o' = "({}+[])[" ++ (number 1) ++ "]"
char 'e' = "({}+[])[" ++ (number 4) ++ "]"
char 'c' = "({}+[])[" ++ (number 5) ++ "]"
char 't' = "({}+[])[" ++ (number 6) ++ "]"
char ' ' = "({}+[])[" ++ (number 7) ++ "]"
char 'f' = "(![]+[])[" ++ (number 0) ++ "]"
char 's' = "(![]+[])[" ++ (number 3) ++ "]"
char 'r' = "(!![]+[])[" ++ (number 1) ++ "]"
char 'u' = "(!![]+[])[" ++ (number 2) ++ "]"
char 'i' = "((+!![]/+[])+[])[" ++ (number 3) ++ "]"
char 'n' = "((+!![]/+[])+[])[" ++ (number 4) ++ "]"
char 'S' = "([]+([]+[])[" ++ (string "constructor") ++ "])[" ++ (number 9) ++ "]"
char 'g' = "([]+([]+[])[" ++ (string "constructor") ++ "])[" ++ (number 14) ++ "]"
char 'p' = "([]+(/-/)[" ++ (string "constructor") ++ "])[" ++ (number 14) ++ "]"
char '\\' = "(/\\\\/+[])[" ++ (number 1) ++ "]"
char 'd' = "(" ++ (number 13) ++ ")[" ++ (string "toString") ++ "]" ++ "(" ++ (number 14) ++ ")"
char 'h' = "(" ++ (number 17) ++ ")[" ++ (string "toString") ++ "]" ++ "(" ++ (number 18) ++ ")"
char 'm' = "(" ++ (number 22) ++ ")[" ++ (string "toString") ++ "]" ++ "(" ++ (number 23) ++ ")"
char 'C' = "(()=>{})[" ++ (string "constructor") ++ "]" ++ "(" ++ (string "return escape") ++ ")()(" ++ (char '\\') ++ ")"
char ch = "([]+[])[" ++ (string "constructor") ++ "]" ++ "[" ++ (string "fromCharCode") ++ "]" ++ "(" ++ (number (ord ch)) ++ ")"

main :: IO ()
main = do
    args <- getArgs
    src <- readFile (head args)
    print (concat (map char src))