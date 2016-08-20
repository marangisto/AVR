module Main (main) where

import Control.Monad

stepsPerRev :: Int
stepsPerRev = 200

maxRPS :: Double
maxRPS = 2.7

minFreq :: Double
minFreq = 10

maxFreq :: Double
maxFreq = maxRPS * fromIntegral stepsPerRev

minStepTime :: Double
minStepTime = 1 / maxFreq

maxStepTime :: Double
maxStepTime = 1 / minFreq

fCPU :: Double
fCPU = 16e6

prescale :: Int
prescale = 64

-- | timer counter value for given time interval
count :: Double -> Int
count t = round $ t * fCPU / fromIntegral prescale

-- | constant-accelleration time
time :: Double -> Int -> Double
time c l = sqrt(2 * fromIntegral l / c)

-- | acceleration required to reach minumum time in specified number of steps
accel :: Int -> Double
accel n = 2 / ((minStepTime / (f (n + 1) - f n))^2)
    where f = sqrt . fromIntegral

computeDt :: Double -> Int -> Double
computeDt c l = time c (l + 1) - time c l

timings :: (Int -> Double) -> Int -> [(Int, Double, Int)]
timings fdt n = [ let dt = fdt l in (l, dt, count dt) | l <- [0..n] ]

main :: IO ()
main = do
    putStrLn $ "minimum step-time = " ++ show minStepTime
    putStrLn $ "maximum timer counter = " ++ show (count maxStepTime)
    putStrLn $ "minimum timer counter = " ++ show (count minStepTime)
    putStrLn $ "accelleration for reaching max speed after 200 steps = " ++ show (accel 200)
    putStrLn $ "accelleration for reaching max speed after 100 steps = " ++ show (accel 100)
    mapM_ print $ let n = 200 in timings (computeDt $ accel n) n
