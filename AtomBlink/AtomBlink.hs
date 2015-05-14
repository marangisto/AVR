module Blink (main) where
 
import Language.Atom
import System.Directory (renameFile)
import Text.PrettyPrint.Mainland
import Arduino
 
main :: IO ()
main = do
    (schedule, _, _, _, _) <- compile name defaults { cCode = prePostCode } blink
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

            period 1000 $ atom "a8" $ do
                call update7
                b8 <== not_ (value b7)
                call update8

            period 20000 $ atom "a9" $ do
                call update9
                b9 <== not_ (value b9)
 
            period 23000 $ atom "a10" $ do
                call update10
                b10 <== not_ (value b10)
 
            period 27000 $ atom "a11" $ do
                call update11
                b11 <== not_ (value b11)
 
            period 24000 $ atom "a12" $ do
                call update12
                b12 <== not_ (value b12)
 
            period 50000 $ atom "a13" $ do
                call update13
                b13 <== not_ (value b13)
 
        (decls, defs, [ update7, update8, update9, update10, update11, update12, update13 ]) = setupPins name pins

        prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
        prePostCode _ _ _ = (pretty 80 $ ppr decls, pretty 80 $ ppr defs)
 
