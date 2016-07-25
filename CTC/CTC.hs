module Main (main) where

import Control.Monad

f_cpu = 16e6

f n x = f_cpu / (2 * n * (1 + x))

g n f = f_cpu / (f * 2 * n) - 1

main :: IO ()
main = do
    forM_ [1, 8, 64, 256, 1024] $ \n -> do
        let fmax = f n 0
            fmin = f n 65535
            res0 = fmax - f n 1
            res1 = f n 65534 - fmin
        putStrLn $ show n ++ " "
                ++ show fmin ++ " "
                ++ show fmax ++ " "
                ++ show res0 ++ " "
                ++ show res1

    forM_ [1, 8, 64, 256, 1024] $ \n -> do
        putStrLn $ show n ++ " "
                ++ show (g n 25e3) ++ " "
                ++ show (g n 100)

