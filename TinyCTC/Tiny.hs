module Main (main) where

f_cpu = 8e6

f n x = f_cpu / (2 * n * (1 + x))

g n f = f_cpu / (f * 2 * n) - 1

main :: IO ()
main = do
    mapM_ (print . g 8) [ 2e3, 2.5e3, 3e3 ]
