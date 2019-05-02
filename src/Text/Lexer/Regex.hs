module Text.Lexer.Regex (
    Regex (..),
    Position
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

type Position = Int
type Positions = S.Set Int

data RegexFunction = RegexFunction {
    nullable  :: M.Map Regex Bool,
    firstpos  :: M.Map Regex Positions,
    lastpos   :: M.Map Regex Positions,
    followpos :: M.Map Position Positions,
    leafsymb  :: M.Map Position Regex
}

-- Post-order traversal
regexFunction :: Regex -> RegexFunction
regexFunction re = execState (run re) (RegexFunction M.empty M.empty M.empty M.empty M.empty) where
    run n@Epsilon =
        modify $ \s -> RegexFunction {
            nullable  = M.insert n True $ nullable s,
            firstpos  = M.insert n S.empty $ firstpos s,
            lastpos   = M.insert n S.empty $ lastpos s,
            followpos = followpos s,
            leafsymb  = leafsymb s
        }
    run n@(Union c1 c2) = do
        run c1
        run c2
        modify $ \s -> RegexFunction {
            nullable  = M.insert n ((nullable s ! c1) || (nullable s ! c2)) $ nullable s,
            firstpos  = M.insert n ((firstpos s ! c1) `S.union` (firstpos s ! c2)) $ firstpos s,
            lastpos   = M.insert n ((lastpos  s ! c1) `S.union` (lastpos  s ! c2)) $ lastpos s,
            followpos = followpos s,
            leafsymb  = leafsymb s
        }
    run n@(Concat c1 c2) = do
        run c1
        run c2
        modify $ \s -> RegexFunction {
            nullable  = M.insert n ((nullable s ! c1) && (nullable s ! c2)) $ nullable s,
            firstpos  = M.insert n ((firstpos s ! c1) `S.union` (if nullable s ! c1 then firstpos s ! c2 else S.empty)) $ firstpos s,
            lastpos   = M.insert n ((lastpos  s ! c2) `S.union` (if nullable s ! c2 then lastpos  s ! c1 else S.empty)) $ lastpos s,
            followpos = S.foldr (M.adjust $ S.union (firstpos s ! c2)) (followpos s) (lastpos s ! c1),
            leafsymb  = leafsymb s
        }
    run n@(Closure c) = do
        run c
        modify $ \s -> RegexFunction {
            nullable  = M.insert n True $ nullable s,
            firstpos  = M.insert n (firstpos s ! c) $ firstpos s,
            lastpos   = M.insert n (lastpos  s ! c) $ lastpos s,
            followpos = S.foldr (M.adjust $ S.union (firstpos s ! n)) (followpos s) (lastpos s ! n),
            leafsymb  = leafsymb s
        }
    run n = do -- Symbol or Endmarker
        poses <- gets leafsymb
        let i = maximum (0 : M.keys poses) + 1
        modify $ \s -> RegexFunction {
            nullable  = M.insert n False $ nullable s,
            firstpos  = M.insert n (S.singleton i) $ firstpos s,
            lastpos   = M.insert n (S.singleton i) $ lastpos s,
            followpos = M.insert i S.empty $ followpos s,
            leafsymb  = M.insert i n $ leafsymb s
        }
