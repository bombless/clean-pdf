module app

import StdEnv
import lexer

Start :: *World -> *World
Start world
    # (console, wolrd)  = stdio world
    # console           = fwrites "Processing file byte by byte:\n" console
    # (ok, file, world) = fopen "test.pdf" FReadData wolrd
    | ok
        # (output, world) = processFileByteByByte file [] world
        # console      = fwrites (toString (length output) +++ "\n") console
        # console      = fwrites (formatTokenList (parse output) +++ "\n") console
        # console      = fwrites "done\n" console
        # (ok, world) = fclose console world
        | ok = world
        | otherwise = abort "console close failed\n"
    | otherwise = abort "file open failed\n"
    where
        processFileByteByByte file acc world
            # (endOfFile, file) = fend file
            | endOfFile
                # (ok, world) = fclose file world
                = (acc, world)
            | otherwise
                # (ok, c, file) = freadc file
                | ok
                    # acc = acc ++ [c]
                    = processFileByteByByte file acc world
                | otherwise = abort "file read failed\n"
