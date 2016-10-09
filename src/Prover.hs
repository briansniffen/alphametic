module Prover where

import Prelude hiding (const,not,and,or)
import SimpleSMT (Solver, SExpr(..), Value(..), Result(..), declare, tInt, assert, int, const, add, mul, eq, gt, lt, not, newSolver, setLogic, check, getConsts, and, or)                 

import Types

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
