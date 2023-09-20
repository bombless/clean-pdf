implementation module lexer

import StdEnv
import lexer
import parseNumber

:: Option a
    = Some a
    | None

toTokensImpl :: [Char] -> (ParseResult Token)
toTokensImpl stream =
    chainParsing stream
        [
            \stream -> tryParseNumber (parseNumber stream),
            match (fromString "<<") DictStart,
            match (fromString ">>") DictEnd,
            match (fromString "[") ListStart,
            match (fromString "]") ListEnd
        ]
where
    match :: [Char] Token -> ([Char] -> (ParseResult Token))
    match x y = \z -> matchYes x y z
    matchYes :: [Char] Token [Char] -> ParseResult Token
    matchYes [] t xs         = ParseOk t xs
    matchYes [x:xs] t [y:ys] = if (x == y) (matchYes xs t ys) (ParseFail [])

    tryParseNumber :: (ParseResult Real) -> (ParseResult Token)
    tryParseNumber (ParseOk rs left) = ParseOk (Number rs) left
    tryParseNumber _                 = ParseFail []
    chainParsing :: [Char] [[Char] -> (ParseResult Token)] -> (ParseResult Token)
    chainParsing stream []     = ParseFail stream
    chainParsing stream [x:xs] = handle (x stream) xs stream
    where
        handle :: (ParseResult Token) [[Char] -> (ParseResult Token)] [Char] -> (ParseResult Token)
        handle (ParseOk rs left) _ _ = ParseOk rs left
        handle (ParseFail left) [] stream = ParseFail stream
        handle (ParseFail left) [x:xs] stream = handle (x stream) xs stream


instance toTokens [Char] where
    toTokens :: [Char] -> [Token]
    toTokens []     = []
    toTokens stream = handle (toTokensImpl stream)
    where
        handle :: (ParseResult Token) -> [Token]
        handle (ParseOk rs []) = [rs]
        handle (ParseOk rs xs) = [rs: handle (toTokensImpl xs)]


instance toTokens {#Char} where
    toTokens :: {#Char} -> [Token]
    toTokens x = parseFromList (fromString x)
    where
        parseFromList :: [Char] -> [Token]
        parseFromList x = toTokens x

instance toString Token where
    toString (StringLiteral _) = "StringLiteral"
    toString (Key _)           = "Key"
    toString DictStart         = "DictStart"
    toString DictEnd           = "DictEnd"
    toString ListStart         = "ListStart"
    toString ListEnd           = "ListEnd"
    toString StreamStart       = "StreamStart"
    toString StreamEnd         = "StreamEnd"
    toString (ObjectStart _ _) = "ObjectStart"
    toString ObjectEnd         = "ObjectEnd"
    toString (Ref _ _)         = "Ref"
    toString (Number n)        = "Number(" +++ toString n +++ ")"
    toString XRef              = "XRef"
    toString Eof               = "Eof"
    toString (Id _)            = "Id"
    toString Null              = "Null"
    toString (Bool b)          = "Bool(" +++ toString b +++ ")"

formatTokenList :: [Token] -> String
formatTokenList x = "[" +++ formatTokenListHelper x +++ "]"
where
    formatTokenListHelper :: [Token] -> String
    formatTokenListHelper [] = ""
    formatTokenListHelper [x] = toString x
    formatTokenListHelper [x:xs] = toString x +++ ", " +++ formatTokenListHelper xs

instance == Token where
    (==) (StringLiteral a) (StringLiteral b) = a == b
    (==) (Key a) (Key b)                     = a == b
    (==) Null Null                           = True
    (==) (Number a) (Number b)               = a == b
    (==) _ _                                 = False

