module EPseudocode.Parser (runLex, mainParser)
where

import Control.Applicative hiding ((<|>))

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
mainParser =
  do reserved tIf
     cond <- expr
     reserved tThen
     thenStmts <- mainParser
     reserved tElse
     elseStmts <- mainParser
     reserved tEndIf
     return $ CompleteIf cond thenStmts elseStmts
  <|>
  do reserved tWhile
     cond <- expr
     reserved tDo
     stmts <- mainParser
     reserved tEndWhile
     return $ While cond stmts
  <|>
  do reserved tFor
     initial <- mainParser
     comma
     cond <- expr
     comma
     iteration <- expr
     reserved tDo
     stmts <- mainParser
     reserved tEndFor
     return $ For initial cond iteration stmts
  <|> (expr >>= return . E) -- TODO: is this the right thing?
