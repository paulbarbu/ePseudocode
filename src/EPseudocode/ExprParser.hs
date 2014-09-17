module EPseudocode.ExprParser (expr)
where

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

import EPseudocode.Data
import EPseudocode.Lexer


expr :: Parser Expr
expr = buildExpressionParser exprTable term
    <?> "expression" -- FIXME: translate


--exprTable :: [[Operator Char () ThrowsError Expr]]
exprTable = [
      [pop "-" UnMinus, pop "!" Not]
    , [iop "*" Mul, iop "/" Div, iop "%" Mod]
    , [iop "+" Plus, iop "-" Minus]
    , [iop "<" Lt, iop "<=" Le, iop ">" Gt, iop ">=" Ge]
    , [iop "==" Eq, iop "!=" Neq]
    , [iop tAnd And]
    , [iop tOr Or]
    ]
    where iop id f= Infix (op id (BinExpr f)) AssocLeft
          pop id f = Prefix $ op id (UnExpr f)
          op id f = reservedOp id >> return f <?> "operator" -- FIXME: translate


term :: Parser Expr
term = parens expr
  <|> liftM Float (try float)
  <|> liftM Int integer
  <|> liftM String stringLiteral
  <|> (reserved tTrue >> return (Bool True))
  <|> (reserved tFalse >> return (Bool False))
  <|> liftM List (braces (commaSep expr))
  <|> try (liftM2 Index identifier (brackets expr))
  <|> liftM Var identifier
  <?> "simple expression" -- FIXME: translate
