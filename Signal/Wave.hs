module Wave (main) where

import Data.List.Split (chunksOf)
import Data.List (intercalate)

gen :: (Double -> Double) -> Double -> Double -> Int -> [Double]
gen f l h n = map (f . g) [0..n-1]
    where g i = l + (fromIntegral i) * (h - l) / fromIntegral n

rebase :: Int -> [Double] -> [Int]
rebase top xs = map (round . f) xs
    where f x = (x - xmin) * fromIntegral top / (xmax - xmin)
          xmin = minimum xs
          xmax = maximum xs

output :: String -> [Int] -> [String]
output name xs = decl : (map ("    "++) $ (zipWith g [0..] $ map f $ chunksOf 32 xs) ++ [ "};", "" ])
    where f = intercalate "," . map show
          g 0 s = "{ " ++ s
          g _ s = ", " ++ s
          decl = "static const uint8_t wave_" ++ name ++ "[] ="

main :: IO ()
main = do
    let range = 255
        tri x | x <= 0.25 = x
              | x <= 0.75 = 0.5 - x
              | otherwise = x - 1
        sqr x | x < 0.5 = 1
              | otherwise = 0;
    mapM_ putStrLn $ output "sin" $ rebase range $ gen sin 0 (2*pi) 256
    mapM_ putStrLn $ output "tri" $ rebase 255 $ gen tri 0 1 256
    mapM_ putStrLn $ output "saw" $ rebase 255 $ gen id 0 1 256
    mapM_ putStrLn $ output "sqr" $ rebase 255 $ gen sqr 0 1 256


