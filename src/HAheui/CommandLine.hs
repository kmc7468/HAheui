module HAheui.CommandLine where

data CommandLine = CommandLine {
    isInterpretingMode :: Bool,
    sourcePath :: String,
    isUTF8, isUTF16, isUTF16BE :: Bool
}

emptyCommandLine = CommandLine False [] False False False

setIsInterpretingMode, setIsUTF8, setIsUTF16, setIsUTF16BE :: CommandLine -> Bool -> CommandLine
setSourcePath :: CommandLine -> String -> CommandLine
setIsInterpretingMode cmdline value = CommandLine value (sourcePath cmdline) (isUTF8 cmdline) (isUTF16 cmdline) (isUTF16BE cmdline)
setSourcePath cmdline value = CommandLine (isInterpretingMode cmdline) value (isUTF8 cmdline) (isUTF16 cmdline) (isUTF16BE cmdline)
setIsUTF8 cmdline value = CommandLine (isInterpretingMode cmdline) (sourcePath cmdline) value (isUTF16 cmdline) (isUTF16BE cmdline)
setIsUTF16 cmdline value = CommandLine (isInterpretingMode cmdline) (sourcePath cmdline) (isUTF8 cmdline) value (isUTF16BE cmdline)
setIsUTF16BE cmdline value = CommandLine (isInterpretingMode cmdline) (sourcePath cmdline) (isUTF8 cmdline) (isUTF16 cmdline) value

type CommandLineWrapper = (Bool, String, CommandLine)
errorCommandLineWrapper message = (False, message, emptyCommandLine)
emptyCommandLineWrapper = errorCommandLineWrapper []

isDone :: CommandLineWrapper -> Bool
output :: CommandLineWrapper -> String
commandLine :: CommandLineWrapper -> CommandLine
isDone (value, _, _) = value
output (_, value, _) = value
commandLine (_, _, value) = value

_parseCommandLine :: [String] -> CommandLineWrapper -> CommandLineWrapper
_parseCommandLine [] cmdline = cmdline
_parseCommandLine (cmd:args) cmdline
    | cmd == "--help" = errorCommandLineWrapper help_message
    | cmd == "--version" = errorCommandLineWrapper version_message
    | cmd == "-i" =
        if isInterpretingMode $ commandLine cmdline
            then errorCommandLineWrapper "오류: -i 옵션이 두번 이상 사용되었습니다."
            else _parseCommandLine args (True, [], setIsInterpretingMode (commandLine cmdline) True)
    | cmd == "-utf8" =
        if isUTF8 $ commandLine cmdline
            then errorCommandLineWrapper "오류: -utf8 옵션이 두번 이상 사용되었습니다."
            else _parseCommandLine args (True, [], setIsUTF8 (commandLine cmdline) True)
    | cmd == "-utf16" =
        if isUTF16 $ commandLine cmdline
            then errorCommandLineWrapper "오류: -utf16 옵션이 두번 이상 사용되었습니다."
            else _parseCommandLine args (True, [], setIsUTF16 (commandLine cmdline) True)
    | cmd == "-utf16be" =
        if isUTF16BE $ commandLine cmdline
            then errorCommandLineWrapper "오류: -utf16be 옵션이 두번 이상 사용되었습니다."
            else _parseCommandLine args (True, [], setIsUTF16BE (commandLine cmdline) True)
    | head cmd == '-' = errorCommandLineWrapper ("오류: " ++ cmd ++ "은/는 알 수 없는 명령입니다.")
    | otherwise =
        if (length $ sourcePath $ commandLine cmdline) > 0
            then errorCommandLineWrapper "오류: sourcePath가 두번 이상 입력되었습니다."
            else _parseCommandLine args (True, [], setSourcePath (commandLine cmdline) cmd)

    where
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
                          \(C) 2018. kmc7468 All rights reserved.\n\
                          \\n\
                          \이 소프트웨어는 공개 소프트웨어로, 아래 웹사이트에서 소스 코드를 다운로드 받을 수 있습니다.\n\
                          \https://github.com/kmc7468/HAheui"

parseCommandLine :: [String] -> CommandLineWrapper
parseCommandLine args = _parseCommandLine args emptyCommandLineWrapper

_checkCommandLineIsInterpretingMode :: CommandLineWrapper -> CommandLineWrapper
_checkCommandLineIsInterpretingMode cmdline =
    if isInterpretingMode $ cmd
        then if isUTF8 $ cmd
            then errorCommandLineWrapper "오류: -i 옵션과 -utf8 옵션은 함께 쓰일 수 없습니다."
            else if isUTF16 $ cmd
                then errorCommandLineWrapper "오류: -i 옵션과 -utf16 옵션은 함께 쓰일 수 없습니다."
                else if isUTF16BE cmd
                    then errorCommandLineWrapper "오류: -i 옵션과 -utf16be 옵션은 함께 쓰일 수 없습니다."
                    else if lenSourcePath > 0
                        then errorCommandLineWrapper "오류: 인터프리팅 모드에서는 sourcePath가 필요하지 않습니다."
                        else cmdline
        else if lenSourcePath == 0
            then errorCommandLineWrapper "오류: 일반 모드에서는 sourcePath가 필요합니다."
            else cmdline

    where
        cmd = commandLine cmdline
        lenSourcePath = length $ sourcePath cmd

_checkCommandLineIsUTF8 :: CommandLineWrapper -> CommandLineWrapper
_checkCommandLineIsUTF8 cmdline =
    if isUTF8 cmd
        then if isUTF16 cmd
            then errorCommandLineWrapper "오류: -utf8 옵션과 -utf16 옵션은 함께 쓰일 수 없습니다."
            else if isUTF16BE cmd
                then errorCommandLineWrapper "오류: -utf8 옵션과 -utf16be 옵션은 함께 쓰일 수 없습니다."
                else cmdline
        else cmdline

    where
        cmd = commandLine cmdline

_checkCommandLineIsUTF16 :: CommandLineWrapper -> CommandLineWrapper
_checkCommandLineIsUTF16 cmdline =
    if isUTF16 cmd
        then if isUTF16BE cmd
            then errorCommandLineWrapper "오류: -utf16 옵션과 -utf16be 옵션은 함께 쓰일 수 없습니다."
            else cmdline
        else cmdline

    where
        cmd = commandLine cmdline

checkCommandLine :: CommandLineWrapper -> CommandLineWrapper
checkCommandLine cmdline =
    if not $ isDone first
        then first
        else if not $ isDone second
            then second
            else third

    where
        first = _checkCommandLineIsInterpretingMode cmdline
        second = _checkCommandLineIsUTF8 cmdline
        third = _checkCommandLineIsUTF16 cmdline