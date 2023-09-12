implementation module parseNumber

import StdEnv

parseNumber :: [Char] -> ParseNumberResult
parseNumber list = handle (parseBeforeDot list)
where
    handle :: ParseNumberResult -> ParseNumberResult
    handle (ParseNumberResultOk n ['.':xs]) = parseAfterDot xs n
    handle (ParseNumberResultOk n xs) = ParseNumberResultOk n xs
    handle x                          = x

parseBeforeDot :: [Char] -> ParseNumberResult
parseBeforeDot list = parseBeforeDotHelper list False 0.0
where
    parseBeforeDotHelper :: [Char] Bool Real -> ParseNumberResult
    parseBeforeDotHelper [] b acc = if b (ParseNumberResultOk acc []) ParseNumberResultNone
    parseBeforeDotHelper [x:xs] b acc
        | x >= '0' && x <= '9' = parseBeforeDotHelper xs True (acc * 10.0 + toReal (toInt (x - '0')))
        | otherwise = if b (ParseNumberResultOk acc [x:xs]) ParseNumberResultNone

parseAfterDot :: [Char] Real -> ParseNumberResult
parseAfterDot list acc = parseAfterDotHelper list acc 0.1
where
    parseAfterDotHelper :: [Char] Real Real -> ParseNumberResult
    parseAfterDotHelper [] acc factor = ParseNumberResultOk acc []
    parseAfterDotHelper [x:xs] acc factor
        | x >= '0' && x <= '9' = parseAfterDotHelper xs (acc + factor * toReal (toInt (x - '0'))) (factor * 0.1)
        | otherwise = ParseNumberResultOk acc [x:xs]
