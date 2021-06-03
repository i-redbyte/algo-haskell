module Main where
import Data.Char
import Data.List
import Data.Maybe

rabinKarp :: [Char] -> [Char] -> Bool
rabinKarp [] [] = True
rabinKarp _ [] = False
rabinKarp a (x:y)
	| roll a == roll (take (length a) (x:y)) = True
	| otherwise = rabinKarp a y

roll :: [Char] -> Int
roll [] = 0
roll (x:y) = (ord x)*7^(length (x:y)-1) + roll y

main = do
    print "Please enter 2 text: "
    text1 <- getLine
    text2 <- getLine
    let dist = rabinKarp text1 text2
    print dist
