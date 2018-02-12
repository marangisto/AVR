module Main where

import Data.Word

--stepsPerOctave = 12 * 8
stepsPerOctave = 12 * 8
maxSamples = 256
minFreq = 27.5 * 2 ** (8/12)
octaves = 7
cpuFreq = 16e6
fudge = 58

computeStep :: Int -> (Int, Double, Int, Int, Int, Int)
computeStep i = (i, freq, stride, samples, count, index)
    where freq = minFreq * 2 ** (fromIntegral i / fromIntegral stepsPerOctave)
          stride = 2 ^ (i `div` stepsPerOctave)
          samples = maxSamples `div` stride
          count = floor $ (cpuFreq - 1) / (freq * fromIntegral samples) + 1
          index = i `mod` stepsPerOctave

main :: IO ()
main = do
    let xs = map computeStep [0..(octaves * stepsPerOctave)]
        ys = [ c | (_, _, _, _, c, _) <- xs ]
    mapM_ print xs
    print $ take stepsPerOctave ys
