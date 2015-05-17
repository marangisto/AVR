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
        pins = [ ("p7",   7, Input,  "b7")
               , ("p8",   8, Output, "b8")
               , ("p9",   9, Output, "b9")
               , ("p10", 10, Output, "b10")
               , ("p11", 11, Output, "b11")
               , ("p12", 12, Output, "b12")
               , ("p13", 13, Output, "b13")
               ]

        blink :: Atom ()
        blink = do
            b7 <- bool "b7" False
            b8 <- bool "b8" False
            b9 <- bool "b9" False
            b10 <- bool "b10" False
            b11 <- bool "b11" False
            b12 <- bool "b12" False
            b13 <- bool "b13" False

            i <- word8 "i" 0

            period 100000 $ atom "a0" $ do
                call update9
                call update10
                call update11
                call update12
                incr i
                b9 <== (value i .&. 1) /=. 0
                b10 <== (value i .&. 2) /=. 0
                b11 <== (value i .&. 4) /=. 0
                b12 <== (value i .&. 8) /=. 0

            period 1000 $ phase 1 $ atom "a8" $ do
                call update7
                call update8
                b8 <== not_ (value b7)

        (decls, defs, [ update7, update8, update9, update10, update11, update12, update13 ]) = setupPins name pins

        prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
        prePostCode _ _ _ = (pretty 80 $ ppr decls, pretty 80 $ ppr defs)
 
