{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }

lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)


term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

eval :: AE -> Int
eval (Num a) = a

evalE :: AE -> Int
evalE (Num a) =
  if (eval (Num a)) < 0
    then error "!"
    else eval (Num a)

evalAE :: AE -> AE
evalAE (Num a) = Num (evalE (Num a))
evalAE (Plus l r) = Num (evalE l + evalE r)
evalAE (Minus l r) =
  if ((evalE l) - (evalE r)) < 0
    then error "!"
    else Num ((evalE l) - (evalE r))
evalAE (Mult l r) = Num (evalE l * evalE r)
evalAE (Div l r) =
  if (evalE r) == 0
    then error "!"
    else Num (div (evalE l) (evalE r))
evalAE (If0 l r s) =
  if (evalE l) == 0
    then evalAE r
    else evalAE s

evalAEMaybe :: AE -> Maybe AE
evalAEMaybe (Num a) =
  if (eval (Num a)) < 0
  then Nothing
  else Just (Num a)
evalAEMaybe (Plus l r) =
  case evalAEMaybe l of
    Just l ->
      case evalAEMaybe r of
        Just r -> Just (Num (eval l + eval r))
        Nothing -> Nothing
    Nothing -> Nothing
evalAEMaybe (Minus l r) =
  case evalAEMaybe l of
    Just l ->
      case evalAEMaybe r of
        Just r -> Just (Num (eval l - eval r))
        Nothing -> Nothing
    Nothing -> Nothing
evalAEMaybe (Mult l r) =
  case evalAEMaybe l of
    Just l ->
      case evalAEMaybe r of
        Just r -> Just (Num (eval l * eval r))
        Nothing -> Nothing
    Nothing -> Nothing
evalAEMaybe (Div l r) =
  case evalAEMaybe l of
    Just l ->
      case evalAEMaybe r of
        Just r ->
          if (eval r) == 0
            then Nothing
            else Just (Num (div (eval l) (eval r)))
        Nothing -> Nothing
    Nothing -> Nothing
evalAEMaybe (If0 l r s) =
  case (eval l) of
    0 -> evalAEMaybe r
    l -> evalAEMaybe s

liftNum :: (Int -> Int -> Int) -> AE -> AE -> AE
liftNum f (Num l) (Num r) = (Num (f l r))

evalM :: AE -> Maybe AE
evalM (Num a) =
  do {
    if (eval (Num a)) < 0 then Nothing else return (Num a)
  }
evalM (Plus l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    return (liftNum (+) x y)
  }
evalM (Minus l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    return (liftNum (-) x y)
  }
evalM (Mult l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    return (liftNum (*) x y)
  }
evalM (Div l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    if eval r == 0
      then Nothing
      else return (liftNum div x y)
  }
evalM (If0 l r s) =
  do {
    x <- evalM l;
    if eval l == 0 then evalM r else evalM s
  }

interpAE :: String -> Maybe AE
interpAE x = evalM (parseAE x)
