module Text.Lexer.Regex (
    Regex (..),
) where

import qualified Text.Lexer.DFA as DFA
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set.Monad as S
import qualified Data.Sequence as Q -- As queue
import Data.Map ((!))
import Data.Sequence (Seq((:<|), (:|>)))
import Control.Monad.State

data Regex = Epsilon
           | Symbol Char
           | Union  Regex Regex
           | Concat Regex Regex
           | Closure Regex
           | Endmark
           deriving (Eq, Ord, Read)

instance Show Regex where
    show = run (Left Epsilon) where
        run _ Epsilon = "ε"
        run _ Endmark = "#"
        run _ (Symbol ch) = [ch]
        run _ (Concat r1 r2) = show r1 ++ show r2
        run _ (Union  r1 r2) = show r1 ++ "|" ++ show r2
        run _ (Closure r) = "(" ++ show r ++ ")*"
