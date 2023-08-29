module app

import StdEnv

Start :: *World -> *World
Start world
    # (console, wolrd)  = stdio world
    # console           = fwrites "Processing file byte by byte:\n" console
    # (ok, file, world) = fopen "test.pdf" FReadData wolrd
    | ok
        # (output, world) = processFileByteByByte file 0 world
        # console      = fwrites (output +++ "\n") console
        # console      = fwrites "done\n" console
        # (ok, world) = fclose console world
        | ok = world
        | otherwise = abort "console close failed\n"
    | otherwise = abort "file open failed\n"
    where
        processFileByteByByte file byteCount world
            # (endOfFile, file) = fend file
            | endOfFile
                # (ok, world) = fclose file world
                = (toString byteCount, world)
            | otherwise
                # (ok, c, file) = freadc file
                | ok
                    # byteCount = byteCount + 1
                    = processFileByteByByte file byteCount world
                | otherwise = abort "file read failed\n"


