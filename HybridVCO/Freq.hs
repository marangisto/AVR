module Main where

import Data.List.Split (chunksOf)
import Data.List (intercalate)
import Data.Word

stepsPerOctave = 12 * 8
maxSamples = 256
minFreq = 27.5 * 2 ** (8/12)
octaves = 7
cpuFreq = 16e6
fudge = 58

computeStep :: Int -> (Int, Double, Int, Int, Int, Int)
computeStep i = (i, freq, stride, samples, count - fudge, index)
    where freq = minFreq * 2 ** (fromIntegral i / fromIntegral stepsPerOctave)
          stride = 2 ^ (i `div` stepsPerOctave)
          samples = maxSamples `div` stride
          count = floor $ (cpuFreq - 1) / (freq * fromIntegral samples) + 1
          index = i `mod` stepsPerOctave

count :: Int -> Int
count i = (floor $ (cpuFreq - 1) / (freq * fromIntegral maxSamples) + 1) - fudge
    where freq = minFreq * 2 ** (fromIntegral i / fromIntegral stepsPerOctave)

output :: [Int] -> [String]
output xs = decl : (map ("    "++) $ (zipWith g [0..] $ map f $ chunksOf 32 xs) ++ [ "};", "" ])
    where f = intercalate "," . map show
          g 0 s = "{ " ++ s
          g _ s = ", " ++ s
          decl = "static const uint16_t counts[] ="

main :: IO ()
main = do
    let xs = map computeStep [0..(octaves * stepsPerOctave)]
        ys = [ c | (_, _, _, _, c, _) <- xs ]
    mapM_ print xs
    -- mapM_ putStrLn $ output $ count 0
    mapM_ putStrLn $ output $ take stepsPerOctave ys
    print $ count 0
