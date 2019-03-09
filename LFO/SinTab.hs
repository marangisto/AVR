module SinTab (main) where

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
          decl = "static const uint8_t " ++ name ++ "_tab[] ="

main :: IO ()
main = do
    mapM_ putStrLn $ output "sin" $ rebase 255 $ gen (\x -> sin x) 0 (pi / 2) 256

