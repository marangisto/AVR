import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util
import Data.Maybe (fromMaybe)

avrgcc = "avr-g++"
avrcopy = "avr-objcopy"
avrdump = "avr-objdump"
atprogram = "atprogram"
avrdude = "avrdude"

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
    , "-I.."
    ]

ldflags =
    [ "-Os"
    , "-Wl,--gc-sections"
    ]

copyflags =
    [ "-Oihex"
    , "-R.eeprom"
    ]

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    usingConfigFile "build.mk"

    want [ buildDir </> "image" <.> "hex", buildDir </> "image" <.> "s" ]

    phony "clean" $ do
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir [ "//*" ]

    buildDir </> "image" <.> "elf" %> \out -> do
        cs <- getDirectoryFiles "" [ "//*.c", "//*.cpp" ]
        mcu <- getMCU
        let os = [ buildDir </> c -<.> "o" | c <- cs ]
        need os
        cmd avrgcc ldflags ("-mmcu=" ++ mcu) "-o" [ out ] os

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
        mcu <- getMCU
        freq <- fmap (fromMaybe "16000000") $ getConfig "F_CPU"
        putNormal $ "MCU=" ++ mcu ++ ", F_CPU=" ++ freq
        () <- cmd avrgcc ccflags ("-mmcu=" ++ mcu) ("-DF_CPU=" ++ freq ++ "L")
                         [ c ] "-o" [ out ] "-MMD -MF" [ m ]
        needMakefileDependencies m

    phony "upload" $ do
        let hex = buildDir </> "image" <.> "hex"
        need [ hex ]
        mcu <- getMCU
        programmer <- fmap (fromMaybe "avrispmk2") $ getConfig "PROGRAMMER"
        putNormal $ "PROGRAMMER=" ++ programmer
        case programmer of
            "avrispmk2" -> cmd atprogram
                 [ "-t", programmer ]
                 [ "-d", mcu ]
                 [ "-i", "isp" ]
                 [ "program", "-c", "--verify", "-f", hex ]
            "arduino" -> cmd avrdude
                 [ "-c" ++ programmer, "-p" ++ mcu, "-P" ++ "COM7" ]
                 [ "-b" ++ "115200", "-v", "-D" ]
                 ("-Uflash:w:" ++ hex ++ ":i")

buildDir = "_build"

getMCU = fmap (fromMaybe "atmega328p") $ getConfig "MCU"

