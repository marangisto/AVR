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
        pins = [("ledPin", 13, Output, "on")]

        blink :: Atom ()
        blink = do
            on <- bool "on" True
            period 40000 $ atom "blinkOn" $ do
                call updateLED
                on <== not_ (value on)
 
        (decls, defs, [updateLED]) = setupPins name pins

        prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
        prePostCode _ _ _ = (pretty 80 $ ppr decls, pretty 80 $ ppr defs)
 
