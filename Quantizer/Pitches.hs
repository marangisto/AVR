module SinTab (main) where

import Data.List.Split (chunksOf)
import Data.List (intercalate)

output :: String -> [Int] -> [String]
output name xs = decl : (map ("    "++) $ (zipWith g [0..] $ map f $ chunksOf 16 xs) ++ [ "};", "" ])
    where f = intercalate "," . map show
          g 0 s = "{ " ++ s
          g _ s = ", " ++ s
          decl = "static const uint16_t " ++ name ++ "_tab[] ="

main :: IO ()
main = do
    mapM_ putStrLn $ output "chromatic" [ let cv = i / 12 in round $ cv * 1000 | i <- [0..49] ]

