module Golf where

-- ex 1. Hopscotch
-- The output of skips is a lists of lists. The first list in the output should
-- be the same as the input list.  The second list in the output should contain
-- every second element from the input list ... and the nth list in the output
-- should contain every nth element from the input list.  Note: the output should
-- be the same length as the input.
-- e.g. skips [1,2,3,4,5,6,7,8,9] == [[1,2,3,4,5,6,7,8,9],  -- every 1
--                                    [2,4,6,8],            -- every 2
--                                    [3,6,9],              -- every 3
--                                    [4,8],                -- every 4
--                                    [5],                  -- every 5
--                                    [6],                  -- every 6
--                                    [7],                  -- every 7
--                                    [8],                  -- every 8
--                                    [9]]                  -- every 9
--      skips "abcde"             == ["abcde",
--                                    "bd",
--                                    "c",
--                                    "d",
--                                    "e"]

skips :: [a] -> [[a]]
skips xs = take (length xs) $ map (`every` xs) [1..]

-- | returns every nth item of a list.
every :: Int -> [a] -> [a]
every _ [] = []
every 0 _  = []
every n xs = take 1 (drop (n - 1) xs) ++ every n (drop n xs)

-- ex 2. Local maxima
-- A local maximum of a list is an element of the list which is strictly greater
-- than both the elements immediately before and after it.  For example, in the
-- list [2,3,4,1,5], the only local maximum is 4, since it is greater than the
-- elements immediately before and after it (3 and 1).  5 is not a local maximum
-- since there is no element after it.  Write a function to find all the local
-- maxima and returns them in order.  (Bonus: generalize to all Ord lists)

localMaxima :: [Integer] -> [Integer]
-- something with:
-- let xs = [a,b,c,d] in zipWith3 f xs (tail xs) (tail (tail xs))
--                                 ^ this part groups adjacent items

-- ex 3. Histogram
-- Write a function which takes an input list of Integers b/t 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number were in the
-- input list.  You may assume that the input list does not contain any numbers
-- less than 0 or reater than 9 (that is, it does not matter what your function
-- does if the input does contain such numbers).  Your output must exactly match
-- this output:                              (note: use putStr to visualize output)
-- histogram [1,1,1,5] ==
--  *
--  *
--  *   *
-- ==========
-- 0123456789                                                              ==
-- unlines [" *        "," *        "," *   *    ","==========","0123456789"]
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789

histogram :: [Integer] -> String
histogram = undefined


