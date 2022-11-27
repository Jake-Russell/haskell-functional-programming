import           Data.List

---- Part 1 ----
steps :: Int -> Int -> Int -> String -- Function Signature
steps n m p = concat[eachStep n (m*p) | p <- [1..p]] -- Creating each step of the pattern

eachStep :: Int -> Int -> String -- Function Signature
eachStep height width = unlines (replicate height (replicate width '*')) -- Creating each line of the pattern
-- The 'unlines' function joines lines, turning a list of strings into a single string with new lines between each


---- Part 2 ----
flagPattern :: Int -> String -- Function Signature
flagPattern flagSize = unlines [eachFlagRow n flagSize | n <- [0..flagSize-1]] -- Creating each line of the pattern

eachFlagRow :: Int -> Int -> String -- Function Signature
eachFlagRow row flagSize
  | row == 0 = replicate flagSize '*' -- If the row has index 0, put a line of *s - This is the top row of the pattern
  | row < (flagSize `div` 2) = concat(["*"] ++ replicate (row - 1)" " ++ ["*"] ++ (replicate (flagSize - 2 - (row * 2))" " ++ ["*"] ++ replicate(row - 1)" ") ++ ["*"]) -- If the row has an index less than the middle row, write this row of *s using the given algorithms
  | row == (flagSize `div` 2) = concat(["*"] ++ (replicate ((flagSize - 3) `div` 2)" " ++ ["*"] ++ replicate((flagSize - 3) `div` 2)" ") ++ ["*"]) -- If the row has an index of the middle row, write this row of *s uisng the given algorithms
  | row < (flagSize - 1) = concat(["*"] ++ replicate (flagSize - row - 2)" " ++ ["*"] ++ replicate ((row * 2) - flagSize)" " ++ ["*"] ++ replicate(flagSize - row - 2)" " ++ ["*"]) -- If the row has an index greater than the middle row, write this row of *s using the given algorithms
  | otherwise = replicate flagSize '*' -- If the row has an index of the flagSize, put a line of *s - This is the bottom row of the pattern
-- The 'replicate' function creates a list of length given by the first argument and the items having the value of the second argument
-- For example, replicate 5 '*' would produce ['*', '*', '*', '*', '*']


---- Part 3 ----
swapwords :: String -> String -> String -> String -- Function Signature
swapwords w1 w2 (word : sentence) = if length (word : sentence) < wordLength -- If the length of the first word of the sentence < the length of the given word ...
                                     then word : sentence -- ... then leave the word being at that positoin in the sentence
                                     else if w1 == take wordLength (word : sentence) -- If the word in the sentence is the same as the given word ...
                                          then w2 ++ (swapwords w1 w2) (drop wordLength (word : sentence)) -- ... then replace the word in the sentence with the given word by recursively calling swapwords until the correct word has been replaced
                                          else word : swapwords w1 w2 sentence -- otherwise continue through the sentence searching for the given word by recursively calling swapwords
                                     where wordLength = length (w1) -- The variable wordLength used throughout is equal to the length of w1
-- The 'take' function creates a list, based on the given arguments. The first argument determines how many itesm should be put in the new list from the list passed in as the second argument
-- For example, take 4 [1,2,3,4,5,6,7,8,9] would produce [1,2,3,4]
-- The 'drop' function does the inverse operation to 'take'. It creates a list of the remaining elements in the passed in list, after removing the number given by the first argument
-- For example, drop 4 [1,2,3,4,5,6,7,8,9] would produce [5,6,7,8,9]


---- Part 4 ----
-- NOTE THIS PART IS NOT COMPLETELY FUNCTIONING
compatibility :: String -> String -> String -- Function Signature
compatibility "" ""       = "" -- Base Case
compatibility name1 name2 = name1 ++ " is " ++ " to " ++ name2 -- Output sentence

removeFirstLetters:: String -> String -> String -- Function Signature
removeFirstLetters name1 name2 = nub name1 \\ nub name2 -- Removing Common Letteres from Name 1

removeSecondLetters:: String -> String -> String -- Function Signature
removeSecondLetters name1 name2 = nub name2 \\ nub name1 -- Removing Common Letters from Name 2

getCompatibility :: String -> String -- Function Signature
getCompatibility name
  | length name `mod` 4 == 0 = "indifferent" -- If the length of the name mod 4 = 0, then the relationship is indifferent
  | length name `mod` 4 == 3 = "hates" -- If the length of the name mod 4 = 3, then the relationship is hates
  | length name `mod` 4 == 2 = "physical" -- If the length of the name mod 4 = 2, then the relationship is physical
  | length name `mod` 4 == 1 = "loves" -- If the length of the name mod 4 = 1, then the relationship is loves
-- The 'nub' function removes duplicate elements from a list. It keeps only the first element of each element.
-- The '\\' function is the list difference. So, used with the 'nub' function, it results in the difference of non-duplicated letters


---- Part 5 ----
split :: (Eq a) => [a] -> a -> [[a]] -- Function Signature
split list character = let (firstHalf, lastHalf) = (break (character ==)) list in firstHalf : -- Let the tuple (firstHalf, lastHalf) equal the split occurances of list in the firstHalf of the tuple (see below for definition and example of break function)
                    if lastHalf == []-- If the last half is an empty list ...
                    then [] -- ... then return an empty list
                    else split (tail lastHalf) character -- Otherwise recursively call split with the tail of the last half of the list
-- The 'break' function creates a tuple of two lists from the original one, separated by a condition boundary
-- For example, break ('a' ==)['b', 'c', 'a', 'd', 'e'] would produce (['b', 'c']['a', 'd', 'e'])
-- The 'let' function acts as a declaration of a variable type. Here, it is declaring a tuple
