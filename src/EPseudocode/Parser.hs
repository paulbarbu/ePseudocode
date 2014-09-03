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
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
whiteSpace = P.whiteSpace lexer
integer = P.integer lexer
float = P.float lexer
stringLiteral = P.stringLiteral lexer


expr :: Parser Expr
expr = buildExpressionParser exprTable term
    <?> "expression" -- FIXME: translate


--exprTable :: [[Operator Char () ThrowsError Expr]]
exprTable = [
      [pop "-" (UnaryOp "-"), pop "!" (UnaryOp "!")]
    , [iop "*" (BinaryOp "*") AssocLeft, iop "/" (BinaryOp "/") AssocLeft, iop "%" (BinaryOp "%") AssocLeft]
    , [iop "+" (BinaryOp "+") AssocLeft, iop "-" (BinaryOp "-") AssocLeft]
    , [iop "<" (BinaryOp "<") AssocLeft, iop "<=" (BinaryOp "<=") AssocLeft, iop ">" (BinaryOp ">") AssocLeft, iop ">=" (BinaryOp ">=") AssocLeft]
    , [iop "==" (BinaryOp "==") AssocLeft, iop "!=" (BinaryOp "!=") AssocLeft]
    , [iop "si" (BinaryOp "si") AssocLeft]
    , [iop "sau" (BinaryOp "sau") AssocLeft]
    ]
    where iop id f assoc = Infix (op id f) assoc
          pop id f = Prefix $ op id f
          op id f = reservedOp id >> return f <?> "operator"


term :: Parser Expr
term = parens expr
  <|> liftM (Ct . Float) (try float)
  <|> liftM (Ct . Int) integer
  <|> liftM (Ct . String) stringLiteral
  <|> (reserved "adevarat" >> return (Ct $ Bool True))
  <|> (reserved "false" >> return (Ct $ Bool False))
  -- TODO: list
  <?> "simple expression"


runLex :: Show a => Parser a -> String -> IO ()
runLex p input = run (whiteSpace *> p <* eof) input


run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "" input of
    Left err -> putStr "parse error at " >> print err
    Right x -> print x
