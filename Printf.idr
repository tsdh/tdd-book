data Format = Number Format
            | Str Format
            | Literal String Format
            | End

%name Format fmt, fmt1, fmt2

PrintfType : Format -> Type
PrintfType (Number fmt) = Integer -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Literal x fmt) = PrintfType fmt
PrintfType End = String

FormatString : Type
FormatString = String

%name FormatString fs, fs1, fs2

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt $ acc ++ show i
printfFmt (Str fmt) acc = \s => printfFmt fmt $ acc ++ s
printfFmt (Literal x fmt) acc = printfFmt fmt $ acc ++ x
printfFmt End acc = acc

parseFormatString : FormatString -> Format
parseFormatString fs = parseFormatString' $ unpack fs
  where parseFormatString' : List Char -> Format
        parseFormatString' [] = End
        parseFormatString' ('%' :: 'd' :: xs) = Number $ parseFormatString' xs
        parseFormatString' ('%' :: 's' :: xs) = Str $ parseFormatString' xs
        parseFormatString' ('%' :: xs) = Literal "%" $ parseFormatString' xs
        parseFormatString' (x :: xs) = Literal (cast x) $ parseFormatString' xs

printf : (fs : FormatString) -> PrintfType $ parseFormatString fs
printf fs = printfFmt (parseFormatString fs) ""
