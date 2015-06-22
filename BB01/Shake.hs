import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

avrgcc = "/Applications/Arduino.app/Contents/Java/hardware/tools/avr/bin/avr-g++"
avrcopy = "/Applications/Arduino.app/Contents/Java/hardware/tools/avr/bin/avr-objcopy"
avrdump = "/Applications/Arduino.app/Contents/Java/hardware/tools/avr/bin/avr-objdump"
avrdude = "/Applications/Arduino.app/Contents/Java/hardware/tools/avr/bin/avrdude"

ccflags =
    [ "-c"
    , "-g"
    , "-Os"
    , "-w"
    , "-std=c++11"
    , "-fno-exceptions"
    , "-ffunction-sections"
    , "-fdata-sections"
    , "-fno-threadsafe-statics"
    , "-MMD"
    , "-mmcu=atmega328p"
    , "-DF_CPU=16000000L"
    , "-I/Applications/Arduino.app/Contents/Java/hardware/arduino/avr/variants/standard"
    ]

ldflags =
    [ "-Os"
    , "-Wl,--gc-sections"
    , "-mmcu=atmega328p"
    ]

copyflags =
    [ "-Oihex"
    , "-R.eeprom"
    ]

dudeflags =
    [ "-C/Applications/Arduino.app/Contents/Java/hardware/tools/avr/etc/avrdude.conf"
    , "-P/dev/cu.usbmodem1d21"
    , "-v"
    , "-cstk500v1"
    , "-patmega328p"
    , "-b19200"
    , "-i25"
    , "-u"
    ]

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

