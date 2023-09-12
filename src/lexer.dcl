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

:: ParseResult a
    = ParseOk a [Char]
    | ParseFail [Char]

class toTokens a :: a -> [Token]

instance toTokens {#Char}

instance toTokens [Char]

formatTokenList :: [Token] -> String

instance == Token
