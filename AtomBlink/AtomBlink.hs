module Blink (main) where
 
import Language.Atom
import System.Directory (renameFile)
import Text.PrettyPrint.Mainland
import Arduino
 
main :: IO ()
main = do
    let config = defaults { cCode = prePostCode, cRuleCoverage = False, cAssert = False }
    (schedule, _, _, _, _) <- compile name config blink
    putStrLn $ reportSchedule schedule
    renameFile (name ++ ".c") (name ++ ".ino") -- Makefile expects .ino
    where
        name = "AtomBlink"

        pins :: [(String, Int, PinType, String)]
        pins = [ ("p2",   2, Output, "sgA")
               , ("p3",   3, Output, "sgB")
               , ("p4",   4, Output, "sgC")
               , ("p5",   5, Output, "sgD")
               , ("p6",   6, Output, "sgE")
               , ("p7",   7, Output, "sgF")
               , ("p8",   8, Output, "sgG")
               , ("p9",   9, Output, "dg1")
               , ("p10", 10, Output, "dg2")
               , ("p11", 11, Output, "dg3")
               , ("p12", 12, Output, "dg4")
               , ("p13", 13, Output, "b13")
               , ("p14", 14, Output, "sDP")
               , ("p15", 15, Input,  "btn")
               ]

        blink :: Atom ()
        blink = do
            [sgA, sgB, sgC, sgD, sgE, sgF, sgG, sDP] <- mapM (`bool` False) [ "sgA", "sgB", "sgC", "sgD", "sgE", "sgF", "sgG", "sDP" ]
            [dg1, dg2, dg3, dg4] <- mapM (`bool` False) [ "dg1", "dg2", "dg3", "dg4" ]
            b13 <- bool "b13" False
            btn <- bool "btn" False

            x <- word16 "x" 0    -- value to display
            i <- word8 "i" 0     -- position
            dp <- word8 "dp" 1   -- decimal point
            ks <- array "ks" [1, 10, 100, 1000]

            let refresh = 250

            period refresh $ phase 0 $ atom "z0" $ do        -- avoid timing artifacts
                mapM_ (<== false) [ dg1, dg2, dg3, dg4 ]

            period refresh $ phase 1 $ atom "z1" $ do        -- avoid timing artifacts
                mapM_ call [ wDg1, wDg2, wDg3, wDg4 ]

            period refresh $ phase 2 $ atom "z2" $ do
                -- sequencing
                let vi = value i
                i <== (vi + 1) .&. 3
                dg1 <== vi ==. 0
                dg2 <== vi ==. 1
                dg3 <== vi ==. 2
                dg4 <== vi ==. 3
                -- segment encoding
                let vx = value x
                    vd = (vx `div_` (ks!.vi)) `mod_` 10
                sgA <== not_ (or_ $ map (vd ==.) [0, 2, 3, 5, 6, 7, 8, 9])
                sgB <== not_ (or_ $ map (vd ==.) [0, 1, 2, 3, 4, 7, 8, 9])
                sgC <== not_ (or_ $ map (vd ==.) [0, 1, 3, 4, 5, 6, 7, 8, 9])
                sgD <== not_ (or_ $ map (vd ==.) [0, 2, 3, 5, 6, 8, 9])
                sgE <== not_ (or_ $ map (vd ==.) [0, 2, 6, 8])
                sgF <== not_ (or_ $ map (vd ==.) [0, 4, 5, 6, 8, 9])
                sgG <== not_ (or_ $ map (vd ==.) [2, 3, 4, 5, 6, 8, 9])
                sDP <== not_ (value dp ==. vi + 1)

            period refresh $ phase 3 $ atom "z3" $ do        -- avoid timing artifacts
                mapM_ call [ wSgA, wSgB, wSgC, wSgD, wSgE, wSgF, wSgG, wSDP ]
                mapM_ call [ wDg1, wDg2, wDg3, wDg4 ]

            period 40000 $ atom "a3" $ do
                dp <== (value dp + 1) `mod_` 5

--            period 4000 $ atom "a2" $ do
--                x <== (value x + 1) `mod_` 10000

            period 100 $ atom "a4" $ do
                mapM_ call [ rBtn, w13 ]
                b13 <== not_ (value btn)

            period 133 $ atom "a5" $ do
                cond (value b13)
                x <== (value x + 1) `mod_` 10000



        ( decls, defs
         , [ wSgA, wSgB, wSgC, wSgD, wSgE, wSgF, wSgG
           , wDg1, wDg2, wDg3, wDg4
           , w13, wSDP, rBtn
           ]
         ) = setupPins name pins

        prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
        prePostCode _ _ _ = (pretty 80 $ ppr decls, pretty 80 $ ppr defs)
 
