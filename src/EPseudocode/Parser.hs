module EPseudocode.Parser (runLex, mainParser)
where

import Control.Applicative hiding ((<|>), many)
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
  <|> liftM List (braces (commaSep (liftM E expr <|> funcDef)))
  <|> try (funcCall <?> "functionc call") -- FIXME: translate -- this occurs twice
  <|> try indexAccess
  <|> liftM Var identifier
  <?> "simple expression" -- FIXME: translate


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
     initial <- mainParser
     semi
     cond <- expr
     semi
     iteration <- assignment
     reserved tDo <?> tDo
     stmts <- many mainParser
     reserved tEndFor <?> tEndFor
     return $ For initial cond iteration stmts
  <|>
  -- return
  do reserved tReturn <?> tReturn
     liftM Ret funcExpr
  <|>
  (funcDef <?> "function definition")  -- FIXME: translate
  <|>
  -- assignment
  try assignment
  <|>
  liftM E expr -- TODO: is this the right thing?


funcExpr :: Parser Stmt
funcExpr = funcDef <|> liftM E expr


funcDef :: Parser Stmt
funcDef = do reserved tFunc
             name <- identifier
             args <- parens (commaSep identifier) <?> "parameters list" -- FIXME: translate
             body <- many mainParser
             reserved tEndFunc
             return $ FuncDef name args body


funcCall :: Parser Expr
funcCall = do name <- try indexAccess <|> try (liftM Var identifier)
              liftM (FuncCall name) (many1 . parens $ commaSep expr) <?> "arguments list" -- FIXME: translate


assignment :: Parser Stmt
assignment = do name <- identifier
                reservedOp "="
                liftM (Assign name) funcExpr


indexAccess :: Parser Expr
indexAccess = liftM2 Index identifier (many1 $ brackets expr)


run :: Show a => Parser a -> String -> String
run p input = case parse p "" input of
    Left err -> "parse error at " ++ show err -- FIXME: translate
    Right x -> show x


runLex :: Show a => Parser a -> String -> String
runLex p input = run (whiteSpace *> many p <* eof) input
