module EPseudocode.Parser (runLex, mainParser)
where

import Control.Applicative hiding ((<|>))
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
runLex p input = run (whiteSpace *> p <* eof) input


mainParser :: Parser Stmt
mainParser = liftM Seq (semiSep1 mainParser')
mainParser' =
  -- complete if
  try (
     do reserved tIf
        cond <- expr
        reserved tThen
        thenStmts <- mainParser
        reserved tElse
        elseStmts <- mainParser
        reserved tEndIf
        return $ CompleteIf cond thenStmts elseStmts
     )
  <|>
  -- simple if
  do reserved tIf
     cond <- expr
     reserved tThen
     thenStmts <- mainParser
     reserved tEndIf
     return $ SimpleIf cond thenStmts
  <|>
  -- while
  do reserved tWhile
     cond <- expr
     reserved tDo
     stmts <- mainParser
     reserved tEndWhile
     return $ While cond stmts
  <|>
  -- for
  do reserved tFor
     initial <- mainParser
     comma
     cond <- expr
     comma
     iteration <- assignment
     reserved tDo
     stmts <- mainParser
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
  liftM E funcCall
  <|>
  (expr >>= return . E) -- TODO: is this the right thing?


funcExpr :: Parser Stmt
funcExpr = do val <- funcDef
              return $ val
           <|>
           do val <- try funcCall <|> expr
              return $ E val


funcDef :: Parser Stmt
funcDef = do reserved tFunc
             name <- identifier
             args <- parens $ commaSep identifier
             body <- mainParser
             -- TODO: allow return here -- via multiple statements: "func x() daca 5 atunci 3 sf daca ret 5 sf func"
             reserved tEndFunc
             return $ FuncDef name args body


funcCall :: Parser Expr
funcCall = do name <- identifier
              liftM (FuncCall name) (parens $ commaSep expr)


assignment :: Parser Stmt
assignment = do name <- identifier
                reservedOp "="
                liftM (Assign name) funcExpr
