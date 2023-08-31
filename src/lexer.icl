implementation module lexer

import StdEnv
import lexer

instance toTokens [Char] where
    toTokens :: [Char] -> [Token]
    toTokens _ = [Null]

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
    toString (Number _)        = "Number"
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

