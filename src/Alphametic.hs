module Alphametic where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Trifecta (parseString, Parser, Result(..), _errDoc)

import Types
import Parser
import Prover

example = "lean + agile + kanban = akamai"
ex2 = (Equal (Prod "money") (Sum [(Prod "send"), (Prod "more")]))

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

