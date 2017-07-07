-- solutions by github.com/aspiringguru

-- split positive integers into list of digits.
-- returns array w digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0 = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10) -- uses recursion


toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []                             -- if <= 0 return empty array
    | otherwise = reverse (toDigitsRev(n))


-- Exercise 2

-- Once we have the digits in the proper order, we need to
-- double every other one. 
-- doubleEveryOther should double every other number beginning
-- from the right, that is, the second-to-last, fourth-to-last
-- numbers are doubled.
doubleEveryOtherFail :: [Integer] -> [Integer]
doubleEveryOtherFail []         = []     -- Do nothing to the empty list
doubleEveryOtherFail (x:[])     = [x]    -- Do nothing to lists with a single element
doubleEveryOtherFail (x:y:zs)     = [x,y+y] ++ doubleEveryOtherFail zs
-- code above fails to pass test

-- NB: odd length - don't double the last object
-- this soution differentiates for even|odd length lists.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n
    | mod (length n)  2 == 0 = zipWith (*) n (cycle [2,1])  --even length do this
    | otherwise = doubleEveryOtherOdd n                     --odd length do this
doubleEveryOtherOdd :: [Integer] -> [Integer]
doubleEveryOtherOdd (x:y:zs)     = [x,y+y] ++ doubleEveryOtherOdd zs
-- NB : zipWith example from 
-- http://zvon.org/other/haskell/Outputprelude/zipWith_f.html
-- todo: simplify solution to one function.


