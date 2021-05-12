module Main where

levenshtein::[Char] -> [Char] -> Int
levenshtein s1 s2
  | length s1 > length s2 = levenshtein s2 s1
  | length s1 < length s2 =
    let d = length s2 - length s1
    in d + levenshtein s1 (take (length s2 - d) s2)
levenshtein "" "" = 0
levenshtein s1 s2
  | last s1 == last s2 = levenshtein (init s1) (init s2)
  | otherwise = minimum [1 + levenshtein (init s1) s2,
                         1 + levenshtein s1 (init s2),
                         1 + levenshtein (init s1) (init s2)]

main = do
    print "Please enter 2 text: "
    text1 <- getLine
    text2 <- getLine
    let dist = levenshtein text1 text2
    print dist

