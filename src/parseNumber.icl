implementation module parseNumber

import StdEnv
import lexer

parseNumber :: [Char] -> (ParseResult Real)
parseNumber list = handle (parseBeforeDot list)
where
    handle :: (ParseResult Real) -> (ParseResult Real)
    handle (ParseOk n ['.':xs]) = parseAfterDot xs n
    handle (ParseOk n xs) = ParseOk n xs
    handle x                          = x

parseBeforeDot :: [Char] -> (ParseResult Real)
parseBeforeDot list = parseBeforeDotHelper list False 0.0
where
    parseBeforeDotHelper :: [Char] Bool Real -> (ParseResult Real)
    parseBeforeDotHelper [] b acc = if b (ParseOk acc []) (ParseFail [])
    parseBeforeDotHelper [x:xs] b acc
        | x >= '0' && x <= '9' = parseBeforeDotHelper xs True (acc * 10.0 + toReal (toInt (x - '0')))
        | otherwise = if b (ParseOk acc [x:xs]) (ParseFail [])

parseAfterDot :: [Char] Real -> (ParseResult Real)
parseAfterDot list acc = parseAfterDotHelper list acc 0.1
where
    parseAfterDotHelper :: [Char] Real Real -> (ParseResult Real)
    parseAfterDotHelper [] acc factor = ParseOk acc []
    parseAfterDotHelper [x:xs] acc factor
        | x >= '0' && x <= '9' = parseAfterDotHelper xs (acc + factor * toReal (toInt (x - '0'))) (factor * 0.1)
        | otherwise = ParseOk acc [x:xs]
