module Wave (main) where


gen :: (Double -> Double) -> Double -> Double -> Int -> [Double]
gen f l h n = map (f . g) [0..n-1]
    where g i = l + (fromIntegral i) * (h - l) / fromIntegral n

rebase :: Int -> [Double] -> [Int]
rebase top xs = map (round . f) xs
    where f x = (x - xmin) * fromIntegral top / (xmax - xmin)
          xmin = minimum xs
          xmax = maximum xs

main :: IO ()
main = do
    print $ rebase 255 $ gen sin 0 (2*pi) 32
    print $ rebase 255 $ gen id 0 1 32
    print $ rebase 255 $ gen (\x -> if x > 1 then 2 - x else x) 0 2 32
    print $ rebase 255 $ gen (\x -> if x > 1 then 1 else 0) 0 2 32


