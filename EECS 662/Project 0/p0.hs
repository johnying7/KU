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
  if a < 0
    then error "!"
    else eval (Num a)

evalAE :: AE -> Int
evalAE (Num a) =
  if a < 0
    then error "!"
    else a
evalAE (Plus l r) = evalAE l + evalAE r
evalAE (Minus l r) =
  if ((evalAE l) - (evalAE r)) < 0
    then error "!"
    else (evalAE l) - (evalAE r)
evalAE (Mult l r) = (evalAE l * evalAE r)
evalAE (Div l r) =
  if (evalAE r) == 0
    then error "!"
    else (div (evalAE l) (evalAE r))
evalAE (If0 l r s) =
  if (evalAE l) == 0
    then evalAE r
    else evalAE s

evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num a) =
  if a < 0
  then Nothing
  else Just a
evalAEMaybe (Plus l r) =
  case evalAEMaybe l of
    Just l ->
      case evalAEMaybe r of
        Just r -> Just (l+r)
        Nothing -> Nothing
    Nothing -> Nothing
evalAEMaybe (Minus l r) =
  case evalAEMaybe l of
    Just l ->
      case evalAEMaybe r of
        Just r -> Just (l-r)
        Nothing -> Nothing
    Nothing -> Nothing
evalAEMaybe (Mult l r) =
  case evalAEMaybe l of
    Just l ->
      case evalAEMaybe r of
        Just r -> Just (l*r)
        Nothing -> Nothing
    Nothing -> Nothing
evalAEMaybe (Div l r) =
  case evalAEMaybe l of
    Just l ->
      case evalAEMaybe r of
        Just r ->
          if r == 0
            then Nothing
            else Just (div l r)
        Nothing -> Nothing
    Nothing -> Nothing
evalAEMaybe (If0 l r s) =
  case (eval l) of
    0 -> evalAEMaybe r
    l -> evalAEMaybe s

liftNum :: (Int -> Int -> Int) -> AE -> AE -> AE
liftNum f (Num l) (Num r) = (Num (f l r))

evalM :: AE -> Maybe Int
evalM (Num a) =
  do {
    if a < 0 then Nothing else return a
  }
evalM (Plus l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    return (x+y)
  }
evalM (Minus l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    return (x-y)
  }
evalM (Mult l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    return (x*y)
  }
evalM (Div l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    if y == 0
      then Nothing
      else return (div x y)
  }
evalM (If0 l r s) =
  do {
    x <- evalM l;
    if x == 0 then evalM r else evalM s
  }

interpAE :: String -> Maybe Int
interpAE x = evalM (parseAE x)

a = (Num 5)
b = (Num (-5))
c = (Num 15)
d = (Num 0)
tests1 = [a, (Plus a c), (Minus c a), (Mult a c), (Div c a), (If0 d a c), (If0 a a c)]
tests2 = [a, b, (Plus a c), (Minus c a), (Minus a c), (Mult a c), (Div c a), (Div a d), (If0 d a c), (If0 a a c)]
tests3 = ["5", "if0 0 then 50 else 20", "if0 5 then 50 else 20", "(-5)"]
--
-- Call these functions for testing
--
validTest1 = map evalAE tests1
validTest2 = map evalAEMaybe tests2
validTest3 = map evalM tests2
validTest4 = map interpAE tests3

-- Invalid Error Message Tests for bang
inv1 = evalAE(b)
inv2 = evalAE(Minus a c)
inv3 = evalAE(Div a d)
