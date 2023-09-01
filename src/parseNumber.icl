implementation module parseNumber

import StdEnv

parseNumber :: [Char] -> ParseNumberResult
parseNumber _ = abort "not implemented"

parseBeforeDot :: [Char] -> ParseNumberResult
parseBeforeDot list = parseBeforeDotHelper list False 0.0
where
    parseBeforeDotHelper :: [Char] Bool Real -> ParseNumberResult
    parseBeforeDotHelper [x:xs] b acc
        | x >= '0' && x <= '9' = parseBeforeDotHelper xs True (acc * 10.0 + toReal (toInt (x - '0')))
        | otherwise = if b (ParseNumberResultOk acc [x:xs]) ParseNumberResultNone

parseAfterDot :: [Char] -> ParseNumberResult
parseAfterDot _ = abort "not implemented"
