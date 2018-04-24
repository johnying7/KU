{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

-- AST Pretty Printer

pprint :: ABE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (Mult n m) = "(" ++ pprint n ++ " * " ++ pprint m ++ ")"
pprint (Div n m) = "(" ++ pprint n ++ " / " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"


-- Parser (Requires ParserUtils and Parsec)

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

expr :: Parser ABE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "*" Plus AssocLeft
            , inFix "/" Minus AssocLeft ]
          , [ inFix "+" Plus AssocLeft
            , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

-- Parser invocation

parseABE = parseString expr

-- Evaluation Functions

liftNum :: (Int -> Int -> Int) -> ABE -> ABE -> ABE
liftNum f (Num l) (Num r) = (Num (f l r))

liftBool :: (Bool -> Bool -> Bool) -> ABE -> ABE -> ABE
liftBool f (Boolean l) (Boolean r) = (Boolean (f l r))

lift :: (Int -> Int -> Bool) -> ABE -> ABE -> ABE
lift f (Num l) (Num r) = (Boolean (f l r))

eval :: ABE -> Int
eval (Num a) = a

evalM :: ABE -> (Maybe ABE)
evalM (Num a) =
  do {
    if a < 0 then Nothing else return (Num a)
  }
evalM (Boolean a) = Just (Boolean a)
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
    if evalM (IsZero r) == evalM (Boolean True)
      then Nothing
      else return (liftNum div x y)
  }
evalM (And l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    return (liftBool (&&) x y)
  }
evalM (Leq l r) =
  do {
    x <- evalM l;
    y <- evalM r;
    return (lift (<=) x y)
  }
evalM (IsZero a) =
  do {
    x <- evalM a;
    if x == (Num 0)
      then return (Boolean True)
      else return (Boolean False)
  }
evalM (If l r s) =
  do {
    x <- evalM l;
    if x == (Boolean True) then evalM r else evalM s
  }

evalErr :: ABE -> (Maybe ABE)
evalErr (Num a) = (return (Num a))
evalErr (Boolean a) = (return (Boolean a))
evalErr (Plus l r) =
  do {
    x <- evalErr l;
    y <- evalErr r;
    case x of
      (Num a) -> case y of
        (Num b) -> Just (Num (a+b))
        _ -> Nothing
      _ -> Nothing
  }
evalErr (Minus l r) =
  do {
    x <- evalErr l;
    y <- evalErr r;
    case x of
      (Num a) -> case y of
        (Num b) -> Just (Num (a-b))
        _ -> Nothing
      _ -> Nothing
  }
evalErr (Mult l r) =
  do {
    x <- evalErr l;
    y <- evalErr r;
    case x of
      (Num a) -> case y of
        (Num b) -> Just (Num (a*b))
        _ -> Nothing
      _ -> Nothing
  }
evalErr (Div l r) =
  do {
    x <- evalErr l;
    y <- evalErr r;
    case x of
      (Num a) -> case y of
        (Num 0) -> Nothing
        (Num b) -> Just (Num (a `div` b))
        _ -> Nothing
      _ -> Nothing
  }
evalErr (And l r) =
  do {
    x <- evalErr l;
    y <- evalErr r;
    case x of
      (Boolean a) -> case y of
        (Boolean b) -> Just (Boolean (a && b))
        _ -> Nothing
      _ -> Nothing
  }
evalErr (Leq l r) =
  do {
    x <- evalErr l;
    y <- evalErr r;
    case x of
      (Num a) -> case y of
        (Num b) -> Just (Boolean (a <= b))
        _ -> Nothing
      _ -> Nothing
  }
evalErr (IsZero a) =
  do {
    x <- (evalErr a);
      case x of
        (Num y) -> (return (Boolean (y == 0)))
        _ -> Nothing
  }
evalErr (If l r s) =
  do {
    x <- evalErr l;
    case x of
      (Boolean a) -> if a then (evalErr r) else (evalErr s)
      _ -> Nothing
  }

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num _) = return TNum
typeofM (Boolean _) = return TBool
typeofM (Plus l r) =
  do {
    x <- typeofM l;
    y <- typeofM r;
    if x == TNum && y == TNum then return TNum else Nothing
  }
typeofM (Minus l r) =
  do {
    x <- typeofM l;
    y <- typeofM r;
    if x == TNum && y == TNum then return TNum else Nothing
  }
typeofM (Mult l r) =
  do {
    x <- typeofM l;
    y <- typeofM r;
    if x == TNum && y == TNum then return TNum else Nothing
  }
typeofM (Div l r) =
  do {
    x <- typeofM l;
    y <- typeofM r;
    if x == TNum && y == TNum then return TNum else Nothing
  }
typeofM (And l r) =
  do {
    x <- typeofM l;
    y <- typeofM r;
    if x == TBool && y == TBool then return TBool else Nothing
  }
typeofM (Leq l r) =
  do {
    x <- typeofM l;
    y <- typeofM r;
    if x == TNum && y == TNum then return TBool else Nothing
  }
typeofM (IsZero a) =
  do {
    x <- typeofM a;
    if x == TNum then return TBool else Nothing
  }
typeofM (If l r s) =
  do {
    x <- typeofM l;
    y <- typeofM r;
    z <- typeofM s;
    if x == TBool
      then if y == z
        then Just y
        else Nothing
      else Nothing
  }

-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM (Num a) =
  do {
    if a < 0 then Nothing else return (Num a)
  }
evalTypeM (Boolean a) = Just (Boolean a)
evalTypeM (Plus l r) =
  if typeofM (Plus l r) == Just (TNum)
    then evalM (Plus l r)
    else Nothing
evalTypeM (Minus l r) =
  if typeofM (Minus l r) == Just (TNum)
    then evalM (Minus l r)
    else Nothing
evalTypeM (Mult l r) =
  if typeofM (Mult l r) == Just (TNum)
    then evalM (Mult l r)
    else Nothing
evalTypeM (Div l r) =
  if typeofM (Div l r) == Just (TNum)
    then evalM (Div l r)
    else Nothing
evalTypeM (And l r) =
  if typeofM (And l r) == Just (TBool)
    then evalM (And l r)
    else Nothing
evalTypeM (Leq l r) =
  if typeofM (Leq l r) == Just (TBool)
    then evalM (Leq l r)
    else Nothing
evalTypeM (IsZero a) =
  if typeofM (IsZero a) == Just (TBool)
    then evalM (IsZero a)
    else Nothing
evalTypeM (If l r s) =
  if typeofM (If l r s) == (typeofM r)
    then evalM (If l r s)
    else Nothing

-- Optimizer

optimize :: ABE -> ABE
optimize (Num a) = Num a
optimize (Boolean a) = Boolean a
optimize (Plus x (Num 0)) = optimize x
optimize (Plus x y) = (Plus (optimize x) (optimize y))
optimize (Minus x y) = (Minus (optimize x) (optimize y))
optimize (Mult x y) = (Mult (optimize x) (optimize y))
optimize (Div x y) = (Div (optimize x) (optimize y))
optimize (And x y) = (And (optimize x) (optimize y))
optimize (Leq x y) = (Leq (optimize x) (optimize y))
optimize (IsZero a) = (IsZero (optimize a))
optimize (If (Boolean True) x y) = optimize x
optimize (If (Boolean False) x y) = optimize y
optimize (If x y z) = (If x y z)

interpOptM :: ABE -> Maybe ABE
interpOptM (Num a) =
  do {
    if a < 0 then Nothing else return (Num a)
  }
interpOptM (Boolean a) = Just (Boolean a)
interpOptM (Plus l r) =
  if typeofM (Plus l r) == Just (TNum)
    then evalM (optimize(Plus l r))
    else Nothing
interpOptM (Minus l r) =
  if typeofM (Minus l r) == Just (TNum)
    then evalM (optimize(Minus l r))
    else Nothing
interpOptM (Mult l r) =
  if typeofM (Mult l r) == Just (TNum)
    then evalM (optimize(Mult l r))
    else Nothing
interpOptM (Div l r) =
  if typeofM (Div l r) == Just (TNum)
    then evalM (optimize(Div l r))
    else Nothing
interpOptM (And l r) =
  if typeofM (And l r) == Just (TBool)
    then evalM (optimize(And l r))
    else Nothing
interpOptM (Leq l r) =
  if typeofM (Leq l r) == Just (TBool)
    then evalM (optimize(Leq l r))
    else Nothing
interpOptM (IsZero a) =
  if typeofM (IsZero a) == Just (TBool)
    then evalM (optimize(IsZero a))
    else Nothing
interpOptM (If l r s) =
  if typeofM (If l r s) == (typeofM r)
    then evalM (optimize(If l r s))
    else Nothing

a = (Num 5)
b = (Num (-5))
c = (Num 15)
d = (Num 0)
e = (Boolean True)
f = (Boolean False)
tests1 = [a, b, (Plus a c), (Plus a d), (Minus c a), (Minus a c), (Mult a c), (Mult a e), (Div c a), (Div a d), (Div c d), (If e a c), (If f a c), (IsZero a), (IsZero d), (IsZero f), (And a a), (And a c), (And e e), (And e f), (And a e), (Leq a c), (Leq c a), (Leq a e), (Leq e f)]
tests2 = [a, b, (Plus a c), (Minus c a), (Minus a c), (Mult a c), (Div c a), (Div a d), (Div c d), (If d a c), (If a a c), (IsZero a), (IsZero d), (And e e), (And e f), (Leq a c), (Leq c a)]
--
-- Call these functions for testing
--
validTest1 = map evalM tests2
validTest2 = map evalErr tests1
validTest3 = map typeofM tests1
validTest4 = map evalTypeM tests1
validTest5 = map optimize tests1
validTest6 = map interpOptM tests1

-- Invalid Error
inval1 = map evalM tests1
