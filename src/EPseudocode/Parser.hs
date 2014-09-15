module EPseudocode.Parser (runLex, mainParser)
where

import Control.Applicative hiding ((<|>), many)
import Control.Monad

import Text.ParserCombinators.Parsec

import EPseudocode.Data
import EPseudocode.ExprParser
import EPseudocode.Lexer


run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "" input of
    Left err -> putStr "parse error at " >> print err -- FIXME: translate
    Right x -> print x


runLex :: Show a => Parser a -> String -> IO ()
runLex p input = run (whiteSpace *> many p <* eof) input


mainParser :: Parser Stmt
mainParser =
  -- complete if
  try (
     do reserved tIf <?> tIf
        cond <- expr
        reserved tThen
        thenStmts <- many mainParser
        reserved tElse
        elseStmts <- many mainParser
        reserved tEndIf
        return $ CompleteIf cond thenStmts elseStmts
     )
  <|>
  -- simple if
  do reserved tIf <?> tIf
     cond <- expr
     reserved tThen
     thenStmts <- many mainParser
     reserved tEndIf
     return $ SimpleIf cond thenStmts
  <|>
  -- while
  do reserved tWhile
     cond <- expr
     reserved tDo
     stmts <- many mainParser
     reserved tEndWhile
     return $ While cond stmts
  <|>
  -- for
  do reserved tFor
     initial <- mainParser
     semi
     cond <- expr
     semi
     iteration <- assignment
     reserved tDo
     stmts <- many mainParser
     reserved tEndFor
     return $ For initial cond iteration stmts
  <|>
  -- return
  do reserved tReturn
     liftM Ret funcExpr
  <|>
  funcDef
  <|>
  -- assignment
  try assignment
  <|>
  -- function call
  (liftM E funcCall <?> "function call") -- FIXME: translate
  <|>
  liftM E expr -- TODO: is this the right thing?


funcExpr :: Parser Stmt
funcExpr = funcDef
           <|>
           do val <- try funcCall <|> expr
              return $ E val


funcDef :: Parser Stmt
funcDef = do reserved tFunc
             name <- identifier
             args <- parens (commaSep identifier) <?> "parameters list" -- FIXME: translate
             body <- many mainParser
             reserved tEndFunc
             return $ FuncDef name args body


funcCall :: Parser Expr
funcCall = do name <- identifier
              liftM (FuncCall name) (parens $ commaSep expr) <?> "arguments list" -- FIXME: translate


assignment :: Parser Stmt
assignment = do name <- identifier
                reservedOp "="
                liftM (Assign name) funcExpr
