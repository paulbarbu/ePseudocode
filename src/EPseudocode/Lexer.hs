module EPseudocode.Lexer (parens, braces, identifier, reservedOp, reserved, whiteSpace, integer, float, stringLiteral, commaSep, comma,
  tAnd, tOr, tTrue, tFalse, tReturn, tIf, tThen, tElse, tEndIf, tWhile, tEndWhile, tFor, tEndFor, tDo, tFunc, tEndFunc)
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-- FIXME: translate
tAnd = "si"
tOr = "sau"
tTrue = "adevarat"
tFalse = "fals"
tReturn = "ret"
tIf = "daca"
tThen = "atunci"
tElse = "altfel"
tEndIf = "sf daca"
tWhile = "cat timp"
tEndWhile = "sf cat timp"
tFor = "pt"
tEndFor = "sf pt"
tDo = "executa"
tFunc = "func"
tEndFunc = "sf func"


epcTokens  = emptyDef {
    caseSensitive = True
  , nestedComments = True
  , commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  , reservedOpNames = ["!", "<", "=", ">", ">=", "<=", "==", "!=", "+", "-", "/", "%", "*", tOr, tAnd]
  , reservedNames = [tTrue, tFalse, tReturn, tIf, tThen, tElse, tEndIf, tWhile, tEndWhile, tFor, tEndFor, tFunc, tEndFunc]
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
commaSep = P.commaSep lexer
comma = P.comma lexer
