module app

import StdEnv

Start :: *World -> *World
Start world
    # (console, wolrd)  = stdio world
    # console           = fwrites "Processing file byte by byte:\n" console
    # (ok, file, world) = fopen "test.pdf" FReadData wolrd
    | ok
        # (output, world) = processFileByteByByte file 0 world
        # console      = fwrites output console
        = world
    | otherwise = abort "file open failed\n"
    where
        processFileByteByByte file byteCount world
            # (endOfFile, file) = fend file
            | endOfFile
                # _ = fclose file
                = (toString byteCount, world)
            | otherwise
                # (ok, c, file) = freadc file
                | ok
                    # byteCount = byteCount + 1
                    = processFileByteByByte file byteCount world
                | otherwise = abort "file read failed\n"


