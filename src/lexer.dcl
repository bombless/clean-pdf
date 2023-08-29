definition module lexer

:: Token = StringLiteral [Char]
         | Key [Char]
         | DictStart
         | DictEnd
         | ListStart
         | ListEnd
         | StreamStart
         | StreamEnd
         | ObjectStart Int Int
         | ObjectEnd
         | Ref Int Int
         | Number Real
         | XRef
         | Eof
         | Id [Char]
         | Null
         | Bool Bool

parse :: [Char] -> [Token]

formatTokenList :: [Token] -> String
