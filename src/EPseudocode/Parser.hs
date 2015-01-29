module EPseudocode.Parser (eParse, runParser, mainParser, toplevelParser)
where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Prelude hiding (id)

import Text.ParserCombinators.Parsec hiding (runParser)
import Text.ParserCombinators.Parsec.Expr

import EPseudocode.Data
import EPseudocode.Lexer

expr :: Parser Expr
expr = buildExpressionParser exprTable term
    <?> "expression" -- FIXME: translate


exprTable :: [[Operator Char () Expr]]
exprTable = [
      [riop "." MemberAccess]
    , [riop "**" Pow]
    , [pop "-" UnMinus, pop "!" Not]
    , [iop "*" Mul, iop "/" Div, iop "%" Mod]
    , [iop "+" Plus, iop "-" Minus]
    , [iop "<" Lt, iop "<=" Le, iop ">" Gt, iop ">=" Ge]
    , [iop "==" Eq, iop "!=" Neq]
    , [iop tAnd And]
    , [iop tOr Or]
    ]
    where iop id f= Infix (op id (BinExpr f)) AssocLeft
          riop id f= Infix (op id (BinExpr f)) AssocRight
          pop id f = Prefix $ op id (UnExpr f)
          op id f = reservedOp id >> return f <?> "operator" -- FIXME: translate


term :: Parser Expr
term = parens expr
  <|> liftM Float (try float)
  <|> liftM Int integer
  <|> liftM String stringLiteral
  <|> (reserved tTrue >> return (Bool True))
  <|> (reserved tFalse >> return (Bool False))
  <|> liftM List (braces (commaSep (expr <|> funcDef)))
  <|> try (funcCall <?> "function call") -- FIXME: translate -- this occurs twice
  <|> try indexAccess
  <|> liftM Var identifier
  <?> "simple expression" -- FIXME: translate


toplevelParser :: Parser Stmt
toplevelParser = liftM E funcDef <|> assignment funcExpr <|> typeDef


mainParser :: Parser Stmt
mainParser =
  -- complete if
  try (
     do reserved tIf <?> tIf
        cond <- expr
        reserved tThen <?> tThen
        thenStmts <- many mainParser
        reserved tElse <?> tElse
        elseStmts <- many mainParser
        reserved tEndIf <?> tEndIf
        return $ CompleteIf cond thenStmts elseStmts
     )
  <|>
  -- simple if
  do reserved tIf <?> tIf
     cond <- expr
     reserved tThen <?> tThen
     thenStmts <- many mainParser
     reserved tEndIf <?> tEndIf
     return $ SimpleIf cond thenStmts
  <|>
  -- while
  do reserved tWhile <?> tWhile
     cond <- expr
     reserved tDo <?> tDo
     stmts <- many mainParser
     reserved tEndWhile <?> tEndWhile
     return $ While cond stmts
  <|>
  -- for
  do reserved tFor <?> tFor
     initial <- liftM Just (assignment expr) <|> return Nothing
     semi
     cond <- liftM Just expr <|> return Nothing
     semi
     iteration <- liftM Just (assignment expr) <|> return Nothing
     reserved tDo <?> tDo
     stmts <- many mainParser
     reserved tEndFor <?> tEndFor
     return $ For initial cond iteration stmts
  <|>
  -- return
  do reserved tReturn <?> tReturn
     liftM Ret funcExpr -- TODO: cannot return a function from the main function
  <|>
  do reserved tBreak <?> tBreak
     return Break
  <|>
  (liftM E funcDef <?> "function definition")  -- FIXME: translate
  <|>
  -- assignment
  try (assignment funcExpr)
  <|>
  liftM E expr -- TODO: is this the right thing? (because it allows stuff like: "func foo() 1 sffunc")


typeDef :: Parser Stmt
typeDef = do reserved tStruct
             name <- identifier <?> "structure name"
             body <- many1 (liftM E funcDef <|> assignment funcExpr)
             reserved tEndStruct
             return $ TypeDef name body


funcExpr :: Parser Expr
funcExpr = funcDef <|> expr


funcDef :: Parser Expr
funcDef = do reserved tFunc
             name <- identifier <|> return ""
             args <- parens (commaSep identifier) <?> "parameters list" -- FIXME: translate
             body <- many mainParser
             reserved tEndFunc
             return $ FuncDef name args body


funcCall :: Parser Expr
funcCall = do name <- try indexAccess <|> try (liftM Var identifier)
              liftM (FuncCall name) (many1 . parens $ commaSep (expr <|> funcDef)) <?> "arguments list" -- FIXME: translate


assignment :: Parser Expr -> Parser Stmt
assignment rhsParser = do lval <- try indexAccess <|> try memberAccess <|> liftM Var identifier
                          reservedOp "="
                          liftM (Assign lval) rhsParser


indexAccess :: Parser Expr
indexAccess = liftM2 Index identifier (many1 $ brackets expr)


memberAccess :: Parser Expr
memberAccess = do
    name <- try identifier
    try dot
    e <- try indexAccess <|> try (liftM Var identifier)
    return $ BinExpr MemberAccess (Var name) e


runParser :: Parser Stmt -> String -> String
runParser p input = case eParse p input of
  Left err -> "parse error at " ++ err -- FIXME: translate
  Right x -> show x


eParse :: Parser Stmt -> String -> Error [Stmt]
eParse p input = case parse (whiteSpace *> many1 p <* eof) "" input of
  Left err -> Left $ show err
  Right program -> Right program
