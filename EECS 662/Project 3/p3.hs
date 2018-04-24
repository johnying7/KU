{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- CFAE AST and Type Definitions

data CFAE where
  Num :: Int -> CFAE
  Plus :: CFAE -> CFAE -> CFAE
  Minus :: CFAE -> CFAE -> CFAE
  Lambda :: String -> CFAE -> CFAE
  App :: CFAE -> CFAE -> CFAE
  Id :: String -> CFAE
  If0 :: CFAE -> CFAE -> CFAE -> CFAE
  deriving (Show,Eq)

type Env = [(String,CFAE)]

evalDynCFAE :: Env -> CFAE -> (Maybe CFAE)
evalDynCFAE e (Num x) = Just (Num x)
evalDynCFAE e (Id i) = (lookup i e)
evalDynCFAE e (Plus l r) = do {
  (Num l') <- evalDynCFAE e l;
  (Num r') <- evalDynCFAE e r;
  return (Num (l'+r'))
}
evalDynCFAE e (Minus l r) = do {
  (Num l') <- evalDynCFAE e l;
  (Num r') <- evalDynCFAE e r;
  return (Num (l'-r'))
}
evalDynCFAE e (Lambda i b) = return (Lambda i b)
evalDynCFAE e (App f a) = do {
  v <- evalDynCFAE e a;
  (Lambda i b) <- evalDynCFAE e f;
  evalDynCFAE ((i,v):e) b
}
evalDynCFAE e (If0 c t a) = do {
  (Num x) <- evalDynCFAE e c;
  if x == 0 then (evalDynCFAE e t) else (evalDynCFAE e a)
}


data CFAEValue where
  NumV :: Int -> CFAEValue
  ClosureV :: String -> CFAE -> Env' -> CFAEValue
  deriving (Show,Eq)

type Env' = [(String,CFAEValue)]

evalStatCFAE :: Env' -> CFAE -> (Maybe CFAEValue)
evalStatCFAE e (Num x) = Just (NumV x)
evalStatCFAE e (Id i) = (lookup i e)
evalStatCFAE e (Plus l r) = do {
  (NumV l') <- evalStatCFAE e l;
  (NumV r') <- evalStatCFAE e r;
  return (NumV (l'+r'))
}
evalStatCFAE e (Minus l r) = do {
  (NumV l') <- evalStatCFAE e l;
  (NumV r') <- evalStatCFAE e r;
  return (NumV (l'-r'))
}
evalStatCFAE e (Lambda i b) = return (ClosureV i b e)
evalStatCFAE e (App f a) = do {
  v <- evalStatCFAE e a;
  (ClosureV i b e') <- evalStatCFAE e f;
  evalStatCFAE ((i,v):e') b
}
evalStatCFAE e (If0 c t a) = do {
  (NumV x) <- evalStatCFAE e c;
  if x == 0 then (evalStatCFAE e t) else (evalStatCFAE e a)
}

data CFBAE where
  Num' :: Int -> CFBAE
  Plus' :: CFBAE -> CFBAE -> CFBAE
  Minus' :: CFBAE -> CFBAE -> CFBAE
  Lambda' :: String -> CFBAE -> CFBAE
  App' :: CFBAE -> CFBAE -> CFBAE
  Bind' :: String -> CFBAE -> CFBAE -> CFBAE
  Id' :: String -> CFBAE
  If0' :: CFBAE -> CFBAE -> CFBAE -> CFBAE
  deriving (Show,Eq)

elabCFBAE :: CFBAE -> CFAE
elabCFBAE (Num' x) = (Num x)
elabCFBAE (Id' i) = Id i
elabCFBAE (Plus' l r) = Plus (elabCFBAE l) (elabCFBAE r)
elabCFBAE (Minus' l r) = Minus (elabCFBAE l) (elabCFBAE r)
elabCFBAE (Lambda' i b) = Lambda i (elabCFBAE b)
elabCFBAE (App' f a) = App (elabCFBAE f) (elabCFBAE a)
elabCFBAE (Bind' i v b) = (App (Lambda i (elabCFBAE b)) (elabCFBAE v))
elabCFBAE (If0' c t a) = If0 (elabCFBAE c) (elabCFBAE t) (elabCFBAE a)

evalCFBAE :: Env' -> CFBAE -> (Maybe CFAEValue)
evalCFBAE e x = evalStatCFAE e (elabCFBAE x)

evalDynCFBAE :: Env -> CFBAE -> (Maybe CFAE)
evalDynCFBAE e x = evalDynCFAE e (elabCFBAE x)
tests1 =
  [
    (Bind' "x" (Num' 1)
      (Bind' "f" (Lambda' "y" (Plus' (Id' "x") (Id' "y")))
        (Bind' "x" (Num' 2)
          (App' (Id' "f") (Num' 3))
        )
      )
    ),
    (Bind' "x" (Plus' (Num' 3) (Num' 1))
      (Bind' "f" (Lambda' "y" (Plus' (Id' "x") (Id' "x")))
        (Bind' "x" (Num' 5)
          (App' (Id' "f") (Num' 8))
        )
      )
    )
    -- IsZero (Num 5),
    -- IsZero (Num 0),
    -- And (Boolean True) (Boolean False),
    -- And (Boolean True) (Boolean True),
    -- Plus (Num 3) (Num 1),
    -- Minus (Num 3) (Num 1),
    -- Leq (Num 5) (Num 3),
    -- Leq (Num 3) (Num 5),
    -- If (Boolean True) (Num 1) (Num 2),
    -- If (Boolean False) (Num 1) (Num 2)
  ]

t1 = (map (evalCFBAE []) tests1)
t2 = (map (evalDynCFBAE []) tests1)
