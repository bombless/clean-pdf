implementation module helper

import lexer
import StdEnv

multipleParses :: [{#Char}] -> String
multipleParses list = join (map (formatTokenList o toTokens) list)

join :: [String] -> String
join list = joinHelper list ""
where
    joinHelper :: [String] String -> String
    joinHelper [] acc = acc
    joinHelper [x] acc = acc +++ x
    joinHelper [x:xs] acc = joinHelper xs (acc +++ x +++ "\n")
