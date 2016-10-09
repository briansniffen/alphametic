module Main where

import Prelude hiding (const,not,and,or)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import Text.Parser.Char hiding (alphaNum, anyChar, space)
import Text.Parser.Combinators (sepBy1)
import Text.Parser.Token (symbolic, ident, natural)
import Text.Parser.Token.Style (emptyIdents)
import Text.Trifecta (parseString, Parser, Result(..), _errDoc)
import SimpleSMT (Solver, SExpr(..), Value(..), Result(..), declare, tInt, assert, int, const, add, mul, eq, gt, lt, not, newSolver, setLogic, check, getConsts, and, or)                 
import System.Environment (getArgs)

import Criterion.Main

example = "lean + agile + kanban = akamai"
ex2 = (Equal (Prod "money") (Sum [(Prod "send"), (Prod "more")]))

data Expr = Equal Expr Expr 
          | Sum [Expr]
          | Mul [Expr]
          | Prod [Char]
          | Lit Integer
          deriving (Read, Show, Eq, Ord)

parse :: String -> Text.Trifecta.Result Expr
parse = parseString parseStmt mempty

parseStmt :: Parser Expr
parseStmt = Equal <$> parseExpr <* symbolic '=' <*> parseExpr

parseExpr :: Parser Expr
parseExpr = Sum <$> sepBy1 (parseLit <|> parseMult) (symbolic '+')

parseMult :: Parser Expr
parseMult = Mul <$> sepBy1 (parseLit <|> parseTerm) (symbolic '*')

parseTerm :: Parser Expr
parseTerm = Prod <$> ident emptyIdents

parseLit :: Parser Expr
parseLit = Lit <$> natural

variables :: Expr -> String
variables (Equal e f) = variables e `L.union` variables f
variables (Sum es) = foldr L.union [] $ map variables es
variables (Mul es) = foldr L.union [] $ map variables es
variables (Prod s) = L.nub s
variables (Lit _) = []

-- | We care abotu the initial variables of each term of an expression
-- because they, conventionally, can't be 0: otherwise they wouldn't
-- be written.
initialVariables :: Expr -> String
initialVariables (Equal e f) = initialVariables e `L.union` initialVariables f
initialVariables (Sum es) = foldr L.union [] $ map initialVariables es
initialVariables (Mul es) = foldr L.union [] $ map initialVariables es
initialVariables (Prod s) = [head s]
initialVariables (Lit _) = []

makeAssertion :: Expr -> SExpr
makeAssertion (Equal a b) = eq (makeAssertion a) (makeAssertion b)
makeAssertion (Sum es) = foldl1 add $ map makeAssertion es
makeAssertion (Mul es) = foldl1 mul $ map makeAssertion es
makeAssertion (Prod s) = foldl (\es c -> add (mul (int 10) es) (const [c])) (int 0) s
makeAssertion (Lit n) = int n

setupVariable :: Solver -> Char -> IO ()
setupVariable s cc = do
  declare s c tInt
  assert s (lt (int (-1)) (const c))
  assert s (gt (int 10) (const c))
    where
      c = [cc]

distinct :: [Char] -> SExpr
distinct cs = not $ foldl1 or [eq (const [a]) (const [b]) | a <- cs, b <- cs, a < b]

solve :: Expr -> IO (Maybe [(String,Value)])
solve e = do
  s <- newSolver "z3" ["-smt2","-in"] Nothing
  mapM_ (setupVariable s) (variables e)
  mapM_ (\v -> assert s (lt (int 0) (const [v]))) (initialVariables e)
  assert s $ distinct (variables e)
  assert s $ makeAssertion e
  res <- check s
  case res of
    Sat -> do
      env <- getConsts s $ map (\c -> [c]) $ variables e
      return $ Just env
    Unsat -> do
      print "Unsatisfiable constraints"
      return Nothing
    Unknown -> do
      print "Inconclusive result"
      return Nothing

formatResults :: String -> [(String,Value)] -> String
formatResults inp env = foldr (replaceOne env) "" inp

replaceOne :: [(String, Value)] -> Char -> String -> String
replaceOne env new base = case (lookup [new] env) of
  Nothing -> new:base
  Just (Int n) -> (head $ show n):base

solveExample :: String -> IO String
solveExample e = case parse e of
                   Success expr -> formatResults e . fromMaybe [] <$> solve expr
                   Failure xs -> print (_errDoc xs) >> return ""

main :: IO ()
main = do
  t <- unwords <$> getArgs
  r <- solveExample t
  putStrLn t
  putStrLn r

benchmark :: IO ()
benchmark = 
  defaultMain [bgroup "" [ bench "akamai" $ nfIO (solveExample "lean + agile + kanban = akamai"),
                           bench "money" $ nfIO (solveExample "send + more = money"),
                           bench "six" $ nfIO (solveExample "six + six + six = nine + nine"),
                           bench "danger" $ nfIO (solveExample "cross + roads = danger"),
                           bench "tales" $ nfIO (solveExample "old + salt + told + tall = tales"),
                           bench "lbj" $ nfIO (solveExample "lyndon * b = johnson")
                         ]
              ]
