module Blink (main) where
 
import Language.Atom hiding (name)
import System.Directory (renameFile)
 
name :: Name
name = "AtomBlink"
    
timeout :: Int
timeout = 20000
 
blink :: Atom ()
blink = do
  on <- bool "on" True
  period timeout $ atom "blinkOn" $ do
    call "avr_blink"
    on <== not_ (value on)
 
header :: String
header = unlines [
          "int16_t ledPin = 13;",
          "void avr_blink(void);"
         ]
 
footer :: String
footer = unlines [
          "void setup() {",
          "  pinMode(ledPin, OUTPUT);",
          "}",
          "",
          "void avr_blink() {",
          "  digitalWrite(ledPin, state." ++ name ++ ".on);",
          "}",
          "",
          "void loop() {",
          "  " ++ name ++ "();",
          "}"
         ]
 
prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ = (header, footer)
 
main :: IO ()
main = do
  (schedule, _, _, _, _) <- compile name defaults { cCode = prePostCode } blink
  putStrLn $ reportSchedule schedule
  renameFile (name ++ ".c") (name ++ ".ino") -- Makefile expects .ino

