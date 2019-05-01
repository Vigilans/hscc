module Main where

-- import Text.Parser
-- import Text.Parser.LL
-- import Text.ParserCombinators.Parsec
-- import Data.Either
-- import qualified Data.Map as M
-- import Control.Monad.State

-- expr :: String
-- expr = "id + ( id + id ) * id"

-- main :: IO ()
-- main = do
--     content <- readFile "app/rules.txt"
--     let rules = M.unions (fromRight M.empty . parser <$> lines content)
--         table = makeLL1 rules
--         prods = evalState (run rules table []) ([SNonTerminal StartRule], (Terminal <$> words expr) ++ [Eof])
--     print (showRule <$> prods)
--     where
--         run rules table prods = do
--             (stack, input) <- get
--             if null stack then return prods
--             else do
--                 action <- stepLL1FA rules table
--                 case action of
--                     LL1Prod _ prod -> run rules table (prod:prods)
--                     _ -> run rules table prods

main :: IO String
main = return "This is a parser"
