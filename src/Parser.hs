module Parser (Parser, Result(..), parse, parseStmt, _errDoc) where

import Control.Applicative ((<|>))
import Text.Parser.Char hiding (alphaNum, anyChar, space)
import Text.Parser.Combinators (sepBy1)
import Text.Parser.Token (symbolic, ident, natural)
import Text.Parser.Token.Style (emptyIdents)
import Text.Trifecta (parseString, Parser, Result(..), _errDoc)

import Types

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
