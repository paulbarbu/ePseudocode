{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module EPseudocode.Lexer (parens, braces, identifier, reservedOp, reserved, whiteSpace, integer, float, stringLiteral, commaSep,
  semi, brackets,
  tAnd, tOr, tTrue, tFalse, tReturn, tIf, tThen, tElse, tEndIf, tWhile, tEndWhile, tFor, tEndFor, tDo, tFunc, tEndFunc, tBreak)
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
tEndIf = "sfdaca"
tWhile = "cattimp"
tEndWhile = "sfcattimp"
tFor = "pt"
tEndFor = "sfpt"
tDo = "executa"
tFunc = "func"
tEndFunc = "sffunc"
tBreak = "stop"


epcTokens  = emptyDef {
    caseSensitive = True
  , nestedComments = True
  , commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  --, opStart = opLetter emptyDef
  , opLetter = oneOf "=*"
  --, reservedOpNames = ["!", "<", "=", ">", ">=", "<=", "==", "!=", "+", "-", "/", "%", "*", "**"]
  , reservedNames = [tOr, tAnd, tTrue, tFalse, tReturn, tIf, tThen, tElse, tEndIf, tWhile, tDo, tEndWhile, tFor, tEndFor, tFunc, tEndFunc, tBreak]
}


lexer = P.makeTokenParser epcTokens

parens = P.parens lexer
braces = P.braces lexer
brackets = P.brackets lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
whiteSpace = P.whiteSpace lexer
integer = P.integer lexer
float = P.float lexer
stringLiteral = P.stringLiteral lexer
commaSep = P.commaSep lexer
semi = P.semi lexer
