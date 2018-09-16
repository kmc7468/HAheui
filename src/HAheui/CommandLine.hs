module HAheui.CommandLine(
    Encoding,
    CommandLine, emptyCommandLine,
    setIsInterpretingMode, setSourcePath, setEncoding,
    CommandLineWrapper,
    parseCommandLine
) where

import Data.Either

data Encoding = NoEncoding | UTF8 | UTF16 | UTF16BE deriving (Eq)
toEncodingOption :: Encoding -> String
toEncodingOption NoEncoding = ""
toEncodingOption UTF8 = "-utf8"
toEncodingOption UTF16 = "-utf16"
toEncodingOption UTF16BE = "-utf16be"

data CommandLine = CommandLine {
    isInterpretingMode :: Bool,
    sourcePath :: String,
    encoding :: Encoding
} deriving (Eq)

emptyCommandLine = CommandLine False [] NoEncoding

setIsInterpretingMode :: CommandLine -> Bool -> CommandLine
setSourcePath :: CommandLine -> String -> CommandLine
setEncoding :: CommandLine -> Encoding -> CommandLine
setIsInterpretingMode cmdline value = CommandLine value (sourcePath cmdline) (encoding cmdline)
setSourcePath cmdline value = CommandLine (isInterpretingMode cmdline) value (encoding cmdline)
setEncoding cmdline value = CommandLine (isInterpretingMode cmdline) (sourcePath cmdline) value

type CommandLineWrapper = Either String CommandLine
errorCommandLineWrapper :: String -> CommandLineWrapper
duplicateCommandLineWrapper :: String -> CommandLineWrapper
togetherCommandLineWrapper :: String -> String -> CommandLineWrapper
errorCommandLineWrapper message = Left message
duplicateCommandLineWrapper option = errorCommandLineWrapper ("오류: " ++ option ++ " 옵션이 두번 이상 사용되었습니다.")
togetherCommandLineWrapper option1 option2 = errorCommandLineWrapper ("오류: " ++ option1 ++ " 옵션과 " ++ option2 ++ " 옵션은 함께 쓰일 수 없습니다.")

parseCommandLineInternal :: [String] -> CommandLineWrapper -> CommandLineWrapper
parseCommandLineInternal [] (Right cmdline) =
    if cmdline == emptyCommandLine
        then errorCommandLineWrapper "사용법: ./HAheui --help"
        else if (not $ isInterpretingMode cmdline) && ((length $ sourcePath cmdline) == 0)
            then errorCommandLineWrapper "오류: 일반 모드에서는 sourcePath가 필요합니다."
            else Right cmdline
parseCommandLineInternal [] cmdline = cmdline
parseCommandLineInternal ("--help":args) _ = errorCommandLineWrapper help_message
parseCommandLineInternal ("--version":args) _ = errorCommandLineWrapper version_message
parseCommandLineInternal ("-i":args) (Right cmdline) =
    if isInterpretingMode cmdline
        then duplicateCommandLineWrapper "-i"
        else if (length $ sourcePath cmdline) > 0
            then errorCommandLineWrapper "오류: 인터프리팅 모드에서는 sourcePath가 필요하지 않습니다."
            else if encoding cmdline == NoEncoding
                then parseCommandLineInternal args (Right (setIsInterpretingMode cmdline True))
                else togetherCommandLineWrapper (toEncodingOption $ encoding cmdline) "-i"
parseCommandLineInternal ("-utf8":args) (Right cmdline) =
    if encoding cmdline == NoEncoding
        then if isInterpretingMode cmdline
            then togetherCommandLineWrapper "-i" "-utf8"
            else parseCommandLineInternal args (Right (setEncoding cmdline UTF8))
        else if oldOption == "-utf8"
            then duplicateCommandLineWrapper "-utf8"
            else togetherCommandLineWrapper "-utf8" oldOption
            where
                oldOption = toEncodingOption $ encoding $ cmdline
parseCommandLineInternal ("-utf16":args) (Right cmdline) =
    if encoding cmdline == NoEncoding
        then if isInterpretingMode cmdline
            then togetherCommandLineWrapper "-i" "-utf16"
            else parseCommandLineInternal args (Right (setEncoding cmdline UTF16))
        else if oldOption == "-utf16"
            then duplicateCommandLineWrapper "-utf16"
            else togetherCommandLineWrapper "-utf16" oldOption
            where
                oldOption = toEncodingOption $ encoding $ cmdline
parseCommandLineInternal ("-utf16be":args) (Right cmdline) =
    if encoding cmdline == NoEncoding
        then if isInterpretingMode cmdline
            then togetherCommandLineWrapper "-i" "-utf16be"
            else parseCommandLineInternal args (Right (setEncoding cmdline UTF16BE))
        else if oldOption == "-utf16be"
            then duplicateCommandLineWrapper "-utf16be"
            else togetherCommandLineWrapper "-utf16be" oldOption
            where
                oldOption = toEncodingOption $ encoding $ cmdline
parseCommandLineInternal (('-':cmd):_) (Right cmdline) = errorCommandLineWrapper ("오류: -" ++ cmd ++ " 옵션은 올바르지 않은 옵션입니다.")
parseCommandLineInternal (source:args) (Right cmdline) =
    if (length $ sourcePath cmdline) > 0
        then errorCommandLineWrapper "오류: sourcePath가 두번 이상 입력되었습니다."
        else if isInterpretingMode $ cmdline
            then errorCommandLineWrapper "오류: 인터프리팅 모드에서는 sourcePath가 필요하지 않습니다."
            else parseCommandLineInternal args (Right (setSourcePath cmdline source))

help_message = "사용법: ./HAheui [option(s)...] [sourcePath]\n\
               \\n\
               \--help  -  모든 명령줄 옵션을 봅니다.\n\
               \--version  -  소프트웨어의 버전을 봅니다.\n\
               \\n\
               \-i  -  소프트웨어를 인터프리팅 모드로 실행합니다. sourcePath가 필요하지 않습니다.\n\
               \-utf8  -  기본적으로 사용되는 옵션입니다. sourcePath가 UTF-8로 인코딩 되었음을 나타냅니다. -i, -utf16, -utf16be 옵션과 함께 쓰일 수 없습니다.\n\
               \-utf16  -  sourcePath가 UTF-16LE로 인코딩 되었음을 나타냅니다. -i, -utf8, -utf16be 옵션과 함께 쓰일 수 없습니다.\n\
               \-utf16be  -  sourcePath가 UTF-16BE로 인코딩 되었음을 나타냅니다. -i, -utf8, -utf16 옵션과 함께 쓰일 수 없습니다."
version_message = "HAheui 1.0.0\n\
                  \(C) 2018. kmc7468 All fromRight reserved.\n\
                  \\n\
                  \이 소프트웨어는 공개 소프트웨어로, 아래 웹사이트에서 소스 코드를 다운로드 받을 수 있습니다.\n\
                  \https://github.com/kmc7468/HAheui"

parseCommandLine :: [String] -> CommandLineWrapper
parseCommandLine args = parseCommandLineInternal args (Right emptyCommandLine)