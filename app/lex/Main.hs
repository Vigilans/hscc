module Main where

import System.IO

main :: IO ()
main = do
    inh <- openFile "./test.l" ReadMode
    inpStr <- hGetContents inh
    putStr inpStr
    hClose inh
    return ()
