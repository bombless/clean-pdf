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

class parse a :: a -> [Token]

instance parse {#Char}

instance parse [Char]

formatTokenList :: [Token] -> String

instance == Token
