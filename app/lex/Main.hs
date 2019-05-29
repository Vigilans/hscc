module Main where

import System.IO
import System.Environment
import Text.Lexer

main :: IO ()
main = do
    [target] <- getArgs
    let iFile = target ++ ".l"
    let oFile = target ++ ".hs"
    input <- readFile iFile
    putStrLn "Successfully read grammar file. Converting it to lexer..."
    let lexer = buildLexer (iFile, input)
    writeFile oFile lexer
    putStrLn $ "Successfully dumped lexer file to " ++ oFile ++ "."
