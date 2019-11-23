module Language.C.Syntax.Utils where

import Language.C.Syntax
import Data.Either

-- Type

type TypeInfo = Either (Type -> Type) Type

void :: Type
void = Void

bool :: Type
bool = Integer Bool Unsigned

char :: Type
char = Integer Char Unsigned

short :: Type
short = Integer Short Signed

int :: Type
int = Integer Int Signed

long :: Type
long = Integer Long Signed

half :: Type
half = Floating Half

float :: Type
float = Floating Float

double :: Type
double = Floating Double

stringType :: String -> Type
stringType s = Array char (fromIntegral $ length s)

computeType :: [TypeInfo] -> Type
computeType infos
    | length primitives > 1 = error "More than one primitive type"
    | otherwise = foldr1 (.) (specifiers ++ primitives) Unknown -- Deduce start from never
    where
        specifiers = lefts infos
        primitives = const <$> rights infos
