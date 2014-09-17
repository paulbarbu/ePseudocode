module EPseudocode.Parser (runLex, mainParser)
where

import Control.Applicative hiding ((<|>), many)
import Control.Monad

import Text.ParserCombinators.Parsec

import EPseudocode.Data
import EPseudocode.ExprParser
import EPseudocode.Lexer


run :: Show a => Parser a -> String -> String
run p input = case parse p "" input of
    Left err -> "parse error at " ++ show err -- FIXME: translate
    Right x -> show x


runLex :: Show a => Parser a -> String -> String
runLex p input = run (whiteSpace *> many p <* eof) input


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


assignment :: Parser Stmt
assignment = do name <- identifier
                reservedOp "="
                liftM (Assign name) funcExpr
