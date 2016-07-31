module Main (main) where

f_cpu = 8e6
freq n c = f_cpu / (2 * n * (1 + c))
count n f = f_cpu / (f * 2 * n) - 1

main :: IO ()
main = mapM_ (print . round . count 8) [ 2e3, 2.5e3, 3e3 ]

