module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | https://www.hackerrank.com/challenges/diagonal-difference/problem
diagonalDifference :: IO ()
diagonalDifference = interact $ compute . map read . words
  where
    compute :: [Int] -> String
    compute (count:matrix) =
      let
        lrSum = sum $ lrDiagonal count matrix
        rlSum = sum $ rlDiagonal count matrix
      in
      show $ abs $ lrSum - rlSum
    compute _ = "Invalid input"
    lrDiagonal :: Int -> [Int] -> [Int]
    lrDiagonal count (x:xs) = x : every (count + 1) xs
    rlDiagonal :: Int -> [Int] -> [Int]
    rlDiagonal count (x:xs) = init $ every (count - 1) xs
    -- Takes every nth element from a list
    -- See https://stackoverflow.com/a/57480917/4407321
    every :: Int -> [a] -> [a]
    every _ [] = []
    every n (x:xs) = every' n (n - 1) (x : xs)
      where
        every' n c []     = []
        every' n 0 (x:xs) = x : every' n (n - 1) xs
        every' n c (x:xs) = every' n (c - 1) xs
