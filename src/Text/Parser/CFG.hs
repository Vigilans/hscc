module Text.Parser.CFG where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set.Monad as S
import Data.Map ((!))
import Data.Set.Monad ((\\))

data Symbol t nt = Endmark | Terminal t | NonTerminal nt deriving (Eq, Ord, Show)

type Production t nt = (nt, [Symbol t nt])

data CFG t nt = CFG {
    -- Context Free Grammar 4-tuple
    terminals    :: S.Set t,
    nonTerminals :: S.Set nt,
    startSymbol  :: nt,
    productions  :: M.Map nt [[Symbol t nt]],

    -- Context Free Grammar functions
    symbols :: S.Set (Symbol t nt),
    first   :: [Symbol t nt] -> S.Set (Maybe t), -- Nothing represents ε
    follow  :: nt -> S.Set (Maybe t) -- Nothing represents $ (nothing follows nt)
}

(|*>) :: (Ord t) => S.Set (Maybe t) -> S.Set (Maybe t) -> S.Set (Maybe t)
s |*> s' | Nothing `elem` s = (s \\ pure Nothing) <> s'
         | otherwise = s

build :: forall t nt . (Ord t, Ord nt) => (nt, [Production t nt]) -> CFG t nt
build (startSymbol, rules) = CFG{..} where
    -- Build definition
    (terminals, nonTerminals, productions) = foldl (\(ts, nts, prods) (head, body) -> (
        ts  <> S.fromList [t | Terminal t <- body],
        nts <> S.fromList (head : [nt | NonTerminal nt <- body]),
        M.insertWith (++) head [body] prods
        )) (S.empty, S.empty, M.empty) rules

    -- Build functions
    symbols :: S.Set (Symbol t nt)
    symbols = (Terminal <$> terminals) <> (NonTerminal <$> nonTerminals)

    {- first ε = {ε}
       first Tα | T is terminal = {T}
                | T is nonterminal and ε ∈ ∪ ∀ T -> β. first β = ∪ ∀ T -> β. first β ∪ first α
                | T is nonterminal and ε ∉ ∪ ∀ T -> β. first β = ∪ ∀ T -> β. first β            -}
    first :: [Symbol t nt] -> S.Set (Maybe t)
    first [] = pure Nothing -- first ε = {ε}
    first (Terminal t : _) = pure (Just t)
    first [NonTerminal nt] = mconcat (first <$> productions ! nt)
    first (nt : bs) = first [nt] |*> first bs

    {- follow S = {$} -- "S" is start rule
       follow N = ∪ for each production rule N' -> αNβ
                | ε ∈ first β and N ≠ N' = first β \ {ε} ∪ follow N'
                | otherwise = first β \ {ε}                         -}
    follow :: nt -> S.Set (Maybe t)
    follow ((== startSymbol) -> True) = pure Nothing -- follow S = {$}
    follow nt = mconcat ((\(n', bs) -> first bs |*> follow n') <$> prods) where
        bodies = [body | (nt', body) <- rules, nt' /= nt'] -- satisifies N ≠ N'
        prods = foldl (\ps body -> let
            indices = fmap (+1) (L.elemIndices (NonTerminal nt) body)
            betas   = fmap (snd . flip L.splitAt body) indices
            prods   = fmap (nt,) betas
            in prods ++ ps) [] bodies
