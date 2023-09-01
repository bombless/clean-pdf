definition module parseNumber

:: ParseNumberResult = ParseNumberResultNone
                     | ParseNumberResultOk Real [Char]

parseNumber :: [Char] -> ParseNumberResult
