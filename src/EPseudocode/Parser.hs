module EPseudocode.Parser (runLex, expr)
where

import Control.Applicative hiding ((<|>))

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P


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


expr :: Parser Integer
expr = buildExpressionParser exprTable factor
    <?> "expression" -- FIXME: translate


exprTable = [
      [pop "-" negate]
    , [iop "*" (*) AssocLeft, iop "/" div AssocLeft, iop "%" rem AssocLeft]
    , [iop "+" (+) AssocLeft, iop "-" (-) AssocLeft]
    ]
    where iop id f assoc = Infix (op id f) assoc
          pop id f = Prefix $ op id f
          op id f = reservedOp id >> return f <?> "operator"


factor :: Parser Integer
factor = parens expr <|> integer
    <?> "simple expression"


runLex :: Show a => Parser a -> String -> IO ()
runLex p input = run (whiteSpace *> p <* eof) input


run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "" input of
    Left err -> putStr "parse error at " >> print err
    Right x -> print x
