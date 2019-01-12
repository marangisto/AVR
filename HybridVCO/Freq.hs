module Main where

import Data.List.Split (chunksOf)
import Data.List (intercalate)

cpuFreq = 20e6          -- Hz
maxFreq = 279           -- Hz 
minCount = 232          -- ()
minPeriod = 1/maxFreq   -- s
clockTick = 1/cpuFreq   -- s
samples = 256           -- ()
fudge = 50              -- ()
startFreq = 110         -- Hz (half of max practical frequency)
stepsPerOctave = 12 * 8 -- ()
noteStep = 2**(1/stepsPerOctave)

f2c :: Double -> Int
f2c f = round ((1/f) / clockTick / samples) - fudge

output :: [Int] -> [String]
output xs = decl : (map ("    "++) $ (zipWith g [0..] $ map f $ chunksOf 16 xs) ++ [ "};", "" ])
    where f = intercalate "," . map show
          g 0 s = "{ " ++ s
          g _ s = ", " ++ s
          decl = "static const uint16_t counts[] ="
 
main :: IO ()
main = do
    let fs = scanl (\f _ -> f * noteStep) startFreq [1..stepsPerOctave] 
    print fs
    print $ map f2c fs
    putStrLn ""
    mapM_ putStrLn $ output $ map f2c $ init fs

