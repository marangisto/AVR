import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.Char (toLower)

data MCU = Atmega328p | Atmega32u4 | Attiny85 deriving Show
data Board = BB328 | BB85 | Leonardo deriving Show
data Programmer = STK500v1 | AVR109 | AvrIspMkII deriving Show

board = BB85

mcu = case board of
    BB85     -> Attiny85
    BB328    -> Atmega328p
    Leonardo -> Atmega32u4

programmer = case board of
    BB85     -> AvrIspMkII
    BB328    -> STK500v1
    Leonardo -> AVR109

avrgcc = "/Applications/Arduino.app/Contents/Java/hardware/tools/avr/bin/avr-g++"
avrcopy = "/Applications/Arduino.app/Contents/Java/hardware/tools/avr/bin/avr-objcopy"
avrdump = "/Applications/Arduino.app/Contents/Java/hardware/tools/avr/bin/avr-objdump"
avrdude = "/Applications/Arduino.app/Contents/Java/hardware/tools/avr/bin/avrdude"

ccflags =
    [ "-c"
    , "-g"
    , "-O3"
    , "-w"
    , "-std=c++11"
    , "-fno-exceptions"
    , "-ffunction-sections"
    , "-fdata-sections"
    , "-fno-threadsafe-statics"
    , "-MMD"
    , "-mmcu=" ++ showLower mcu
    , "-DF_CPU=16000000L"
    , "-I/Applications/Arduino.app/Contents/Java/hardware/arduino/avr/variants/standard"
    ]

ldflags =
    [ "-Os"
    , "-Wl,--gc-sections"
    , "-mmcu=" ++ showLower mcu
    ]

copyflags =
    [ "-Oihex"
    , "-R.eeprom"
    ]

dudeflags =
    [ "-C/Applications/Arduino.app/Contents/Java/hardware/tools/avr/etc/avrdude.conf"
    , "-P/dev/cu.usbmodem1d11"
    , "-v"
    , "-c" ++ showLower programmer
    , "-p" ++ showLower mcu
    ] ++ case programmer of
        STK500v1   -> [ "-b19200", "-i25", "-u" ]
        AVR109     -> [ "-b57600", "-D" ]
        AvrIspMkII -> [ "-Pusb" ]

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    want [ buildDir </> "image" <.> "hex", buildDir </> "image" <.> "s" ]

    phony "clean" $ do
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir [ "//*" ]

    buildDir </> "image" <.> "elf" %> \out -> do
        cs <- getDirectoryFiles "" [ "//*.c", "//*.cpp" ]
        let os = [ buildDir </> c -<.> "o" | c <- cs ]
        need os
        cmd avrgcc ldflags "-o" [ out ] os

    buildDir </> "image" <.> "hex" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        cmd avrcopy copyflags [ elf ] [ out ]

    buildDir </> "image" <.> "s" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        Stdout res <- cmd avrdump "-S" [ elf ]
        writeFile' out res

    buildDir </> "//*.o" %> \out -> do
        let c = dropDirectory1 $ out -<.> "cpp"
            m = out -<.> "m"
        () <- cmd avrgcc ccflags [ c ] "-o" [ out ] "-MMD -MF" [ m ]
        needMakefileDependencies m

    phony "upload" $ do
        let hex = buildDir </> "image" <.> "hex"
        need [ hex ]
        cmd avrdude dudeflags ("-Uflash:w:" ++ hex ++ ":i")

buildDir = "_build"

showLower :: (Show a) => a -> [Char]
showLower = map toLower . show

