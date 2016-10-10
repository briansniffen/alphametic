module Types (Expr(..), variables, initialVariables) where

import qualified Data.List as L

data Expr = Equal Expr Expr 
          | Sum [Expr]
          | Mul [Expr]
          | Prod [Char]
          | Lit Integer
          deriving (Read, Show, Eq, Ord)


variables :: Expr -> String
variables (Equal e f) = variables e `L.union` variables f
variables (Sum es) = foldr L.union [] $ map variables es
variables (Mul es) = foldr L.union [] $ map variables es
variables (Prod s) = L.nub s
variables (Lit _) = []

-- | We care about the initial variables of each term of an expression
-- because they, conventionally, can't be 0: otherwise they wouldn't
-- be written.
initialVariables :: Expr -> String
initialVariables (Equal e f) = initialVariables e `L.union` initialVariables f
initialVariables (Sum es) = foldr L.union [] $ map initialVariables es
initialVariables (Mul es) = foldr L.union [] $ map initialVariables es
initialVariables (Prod s) = [head s]
initialVariables (Lit _) = []

