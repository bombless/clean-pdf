module test

import lexer
import StdEnv

Start
    | parse "1" <> [Number 1.0] = abort "parse \"1\" failed"