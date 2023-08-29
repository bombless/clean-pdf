implementation module lexer

import StdEnv
import lexer

parse :: [Char] -> [Token]
parse _ = [Null]

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
    toString (Bool b)            = "Bool(" +++ toString b +++ ")"

formatTokenList :: [Token] -> String
formatTokenList x = "[" +++ formatTokenListHelper x +++ "]"

formatTokenListHelper :: [Token] -> String
formatTokenListHelper [] = ""
formatTokenListHelper [x] = toString x
formatTokenListHelper [x:xs] = toString x +++ ", " +++ formatTokenListHelper xs
