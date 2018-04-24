{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

liftNum :: (Int -> Int -> Int) -> BBAE -> BBAE -> BBAE
liftNum f (Num l) (Num r) = (Num (f l r))

liftBool :: (Bool -> Bool -> Bool) -> BBAE -> BBAE -> BBAE
liftBool f (Boolean l) (Boolean r) = (Boolean (f l r))

lift :: (Int -> Int -> Bool) -> BBAE -> BBAE -> BBAE
lift f (Num l) (Num r) = (Boolean (f l r))

subst :: String -> BBAE -> BBAE -> BBAE
subst i v (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') =
    if i == i'
      then (Bind i' (subst i v v') b')
      else (Bind i (subst i v v') (subst i v b'))
subst i v (Id i') =
  if i == i'
    then v
    else (Id i')
subst i v (Boolean x) = (Boolean x)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero x) = (IsZero (subst i v x))
subst i v (If l r s) = (If (subst i v l) (subst i v r) (subst i v s))

evalS :: BBAE -> (Maybe BBAE)
evalS (Num x) = Just (Num x)
evalS (Plus l r) =
  do {
    (Num l') <- (evalS l);
    (Num r') <- (evalS r);
    return (Num (l'+r'))
  }
evalS (Minus l r) =
  do {
    (Num l') <- (evalS l);
    (Num r') <- (evalS r);
    return (Num (l'-r'))
  }
evalS (Bind i v b) =
  do {
    v' <- (evalS v);
    (evalS (subst i v' b))
  }
evalS (Id n) = Nothing
evalS (Boolean x) = Just (Boolean x)
evalS (And l r) =
  do {
    (Boolean l') <- evalS l;
    (Boolean r') <- evalS r;
    return (Boolean (l'&&r'))
  }
evalS (Leq l r) =
  do {
    (Num l') <- evalS l;
    (Num r') <- evalS r;
    return (Boolean (l'<=r'))
  }
evalS (IsZero a) =
  do {
    (Num x) <- evalS a;
    if x == 0
      then return (Boolean True)
      else return (Boolean False)
  }
evalS (If l r s) =
  do {
    (Boolean x) <- evalS l;
    if x == (True) then evalS r else evalS s
  }


evalM :: Env -> BBAE -> (Maybe BBAE)
evalM e (Num x) = Just (Num x)
evalM e (Plus l r) =
  do {
    (Num l') <- evalM e l;
    (Num r') <- evalM e r;
    return (Num (l'+r'))
  }
evalM e (Minus l r) =
  do {
    (Num l') <- evalM e l;
    (Num r') <- evalM e r;
    return (Num (l'-r'))
  }
evalM e (Bind i v b) =
  do {
    v' <- evalM e v;
    evalM ((i,v'):e) b
  }
evalM e (Id n) = (lookup n e)
evalM e (Boolean a) = Just (Boolean a)
evalM e (And l r) =
  do {
    (Boolean l') <- evalM e l;
    (Boolean r') <- evalM e r;
    return (Boolean (l'&&r'))
  }
evalM e (Leq l r) =
  do {
    (Num l') <- evalM e l;
    (Num r') <- evalM e r;
    return (Boolean (l'<=r'))
  }
evalM e (IsZero a) =
  do {
    x <- evalM e a;
    if x == (Num 0)
      then return (Boolean True)
      else return (Boolean False)
  }
evalM e (If l r s) =
  do {
    (Boolean x) <- evalM e l;
    if x == True then evalM e r else evalM e s
  }


testBBAE :: BBAE -> Bool
testBBAE x = (evalM [] x == evalS x)

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM c (Num x) = return TNum
typeofM c (Plus l r) =
  do {
    l' <- typeofM c l;
    r' <- typeofM c r;
    if l'==TNum && r'==TNum then return TNum else Nothing
  }
typeofM c (Minus l r) =
  do {
    l' <- typeofM c l;
    r' <- typeofM c r;
    if l'==TNum && r'==TNum then return TNum else Nothing
  }
typeofM c (Bind i v b) =
  do {
    v' <- typeofM c v;
    typeofM ((i,v'):c) b
  }
typeofM c (Id n) = (lookup n c)
typeofM c (Boolean _) = return TBool
typeofM c (And l r) =
  do {
    x <- typeofM c l;
    y <- typeofM c r;
    if x == TBool && y == TBool then return TBool else Nothing
  }
typeofM c (Leq l r) =
  do {
    x <- typeofM c l;
    y <- typeofM c r;
    if x == TNum && y == TNum then return TBool else Nothing
  }
typeofM c (IsZero a) =
  do {
    x <- typeofM c a;
    if x == TNum then return TBool else Nothing
  }
typeofM c (If l r s) =
  do {
    x <- typeofM c l;
    y <- typeofM c r;
    z <- typeofM c s;
    if x == TBool
      then if y == z
        then Just y
        else Nothing
      else Nothing
  }


evalT :: BBAE -> (Maybe BBAE)
evalT x = do {typeofM [] x; evalM [] x}

tests1 =
  [
    Bind "x" (Plus (Num 3) (Num 1)) (Plus (Id "x") (Id "x")),
    IsZero (Num 5),
    IsZero (Num 0),
    And (Boolean True) (Boolean False),
    And (Boolean True) (Boolean True),
    Plus (Num 3) (Num 1),
    Minus (Num 3) (Num 1),
    Leq (Num 5) (Num 3),
    Leq (Num 3) (Num 5),
    If (Boolean True) (Num 1) (Num 2),
    If (Boolean False) (Num 1) (Num 2)
  ]

answers =
  [
    Just (Num 8),
    Just (Boolean False),
    Just (Boolean True),
    Just (Boolean False),
    Just (Boolean True),
    Just (Num 4),
    Just (Num 2),
    Just (Boolean False),
    Just (Boolean True),
    Just (Num 1),
    Just (Num 2)
  ]

t1 = if all (==True) (map testBBAE tests1)
  then
    "All are True"
  else
    "Something Wrong"
t2 = (map evalT tests1)
