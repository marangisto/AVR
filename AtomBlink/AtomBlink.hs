module Blink (main) where
 
import Language.Atom
import System.Directory (renameFile)
import Text.PrettyPrint.Mainland
import Arduino
 
main :: IO ()
main = do
    let config = defaults
            { cCode = prePostCode
--            , hardwareClock = Just $ Clock "millis" Word32 1 "delay" Nothing
            , cRuleCoverage = False
            , cAssert = False
            }
    (schedule, _, _, _, _) <- compile name config blink
    putStrLn $ reportSchedule schedule
    renameFile (name ++ ".c") (name ++ ".ino") -- Makefile expects .ino
    where
        name = "AtomBlink"

        pins :: [(String, Int, PinType, String)]
        pins = [ ("p2",   2, Output, "b2")
               , ("p3",   3, Output, "b3")
               , ("p4",   4, Output, "b4")
               , ("p5",   5, Output, "b5")
               , ("p6",   6, Output, "b6")
               , ("p7",   7, Output, "b7")
               , ("p8",   8, Output, "b8")
               , ("p9",   9, Output, "b9")
               , ("p10", 10, Output, "b10")
               , ("p11", 11, Output, "b11")
               , ("p12", 12, Output, "b12")
               , ("p13", 13, Output, "b13")
               ]

        blink :: Atom ()
        blink = do
            b2 <- bool "b2" False
            b3 <- bool "b3" False
            b4 <- bool "b4" False
            b5 <- bool "b5" False
            b6 <- bool "b6" False
            b7 <- bool "b7" False
            b8 <- bool "b8" False
            b9 <- bool "b9" False
            b10 <- bool "b10" False
            b11 <- bool "b11" False
            b12 <- bool "b12" False
            b13 <- bool "b13" False

            x <- word16 "x" 0    -- value to display
            i <- word8 "i" 0     -- position
            d <- word16 "d" 0     -- current digit
            ks <- array "ks" [10, 100, 1000, 1]

            period 500 $ atom "a0" $ do
                call update9
                call update10
                call update11
                call update12

                call update2
                call update3
                call update4
                call update5
                call update6
                call update7
                call update8

                -- sequencing
                let vi = value i
                i <== (vi + 1) .&. 3
                b9 <== vi ==. 0
                b10 <== vi ==. 1
                b11 <== vi ==. 2
                b12 <== vi ==. 3
                -- segment encoding
                let vx = value x
                d <== (vx `div_` (ks!.vi)) `mod_` 10
                let vd = value d
                b2 <== not_ (or_ $ map (vd ==.) [0, 2, 3, 5, 6, 7, 8, 9])
                b3 <== not_ (or_ $ map (vd ==.) [0, 1, 2, 3, 4, 7, 8, 9])
                b4 <== not_ (or_ $ map (vd ==.) [0, 1, 3, 4, 5, 6, 7, 8, 9])
                b5 <== not_ (or_ $ map (vd ==.) [0, 2, 3, 5, 6, 8, 9])
                b6 <== not_ (or_ $ map (vd ==.) [0, 2, 6, 8])
                b7 <== not_ (or_ $ map (vd ==.) [0, 4, 5, 6, 8, 9])
                b8 <== not_ (or_ $ map (vd ==.) [2, 3, 4, 5, 6, 8, 9])

            period 10000 $ atom "a1" $ do
                x <== (value x + 1) `mod_` 10000

{-
            period 1000 $ phase 1 $ atom "a8" $ do
                call update7
                call update3
                b8 <== not_ (value b7)
-}

        ( decls
         , defs
         , [ update2
          , update3
          , update4
          , update5
          , update6
          , update7
          , update8
          , update9
          , update10
          , update11
          , update12
          , update13
          ]
         ) = setupPins name pins

        prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
        prePostCode _ _ _ = (pretty 80 $ ppr decls, pretty 80 $ ppr defs)
 
