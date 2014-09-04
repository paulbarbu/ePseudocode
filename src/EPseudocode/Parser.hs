module EPseudocode.Parser (runLex, expr)
where

import Control.Applicative hiding ((<|>))
import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P

import EPseudocode.Data


epcTokens  = emptyDef {
    caseSensitive = True
  , nestedComments = True
  , commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  , reservedOpNames = ["!", "<", "=", ">", ">=", "<=", "==", "!=", "+", "-", "/", "%", "*", "sau", "si"]
  , reservedNames = ["adevarat", "fals", "ret", "daca", "atunci", "altfel", "sf daca", "cat timp", "sf cat timp", "pt", "sf pt",
    "func", "sf func"] --FIXME: translate
}


lexer = P.makeTokenParser epcTokens
parens = P.parens lexer
braces = P.braces lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
whiteSpace = P.whiteSpace lexer
integer = P.integer lexer
float = P.float lexer
stringLiteral = P.stringLiteral lexer
commaSep1 = P.commaSep1 lexer


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
    , [iop "si" And]
    , [iop "sau" Or]
    ]
    where iop id f= Infix (op id (BinExpr f)) AssocLeft
          pop id f = Prefix $ op id (UnExpr f)
          op id f = reservedOp id >> return f <?> "operator"


term :: Parser Expr
term = parens expr
  <|> liftM Float (try float)
  <|> liftM Int integer
  <|> liftM String stringLiteral
  <|> (reserved "adevarat" >> return (Bool True))
  <|> (reserved "false" >> return (Bool False))
  <|> liftM List (braces (commaSep1 expr))
  <|> liftM Var identifier
  <?> "simple expression"


runLex :: Show a => Parser a -> String -> IO ()
runLex p input = run (whiteSpace *> p <* eof) input


run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "" input of
    Left err -> putStr "parse error at " >> print err
    Right x -> print x
