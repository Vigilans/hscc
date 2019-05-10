module Text.Lexer.LexParser (
    parse
) where

import System.IO
type Action = String
data ParseResult = ParseResult {
    regexes             :: [(String, Action)],
    definitionSection   :: String,
    userSubroutines     :: String
}
parse :: String -> ParseResult
parse s = undefined

main = do
    inh <- openFile "./test.l" ReadMode
    inpStr <- hGetContents inh
    putStr inpStr
    hClose inh