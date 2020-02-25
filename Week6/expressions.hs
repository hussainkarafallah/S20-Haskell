{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- expressions extended with 2 operations as requested in the homework
-- exponentiation and subtraction
data Expr a
  = Lit Integer -- ^ Integer literal.
  | Var a -- ^ Variable
  | Add (Expr a) (Expr a) -- ^Addition.  
  | Mul (Expr a) (Expr a) -- ^Multiplication.
  | Sub (Expr a) (Expr a) -- ^Subtraction
  | Pow (Expr a) (Expr a) -- ^Exponentiation
    deriving (Show, Functor)

instance Num (Expr a) where
  e1 + e2 = Add e1 e2
  e1 * e2 = Mul e1 e2
  e1 - e2 = Sub e1 e2
  fromInteger n = Lit n

-- a function that finds a pair using the first as key and returns the second
-- with support of returning a default
findWithDefault :: Eq a => b -> [(a,b)] -> a -> b
findWithDefault def list key = case (lookup key list) of
  Nothing -> def
  Just val -> val


-- the function written in the assignment
evalWith :: Eq var => Integer -> [(var, Integer)] -> Expr var -> Integer
evalWith def list (Lit x) = x
evalWith def list (Var x) = case (lookup x list) of
  Nothing -> def
  Just val -> val
evalWith def list (Add e1 e2) = evalWith def list e1 + evalWith def list e2
evalWith def list (Sub e1 e2) = evalWith def list e1 - evalWith def list e2
evalWith def list (Mul e1 e2) = evalWith def list e1 * evalWith def list e2
evalWith def list (Pow e1 e2) = (evalWith def list e1) ^ (evalWith def list e2)

-- the function written in the assignment
displayWith :: (var -> String) -> Expr var -> String
displayWith f (Lit x) = show x
displayWith f (Var x) = f x
displayWith f (Add e1 e2) = "(" ++ displayWith f e1 ++ "+" ++ displayWith f e2 ++ ")"
displayWith f (Sub e1 e2) = "(" ++ displayWith f e1 ++ "-" ++ displayWith f e2 ++ ")"
displayWith f (Mul e1 e2) = "(" ++ displayWith f e1 ++ "*" ++ displayWith f e2 ++ ")"
displayWith f (Pow e1 e2) = "(" ++ displayWith f e1 ++ "^" ++ displayWith f e2 ++ ")"

-- a general definition of expand vars with use of the functor properties
-- it acceps a list of (variable , thing to be replaced with) and a default value
-- and simply converts expressions by substitution from one type to another
-- so it would be possible to have expression of expressions
expandVars :: Eq var => a -> [(var , a)] -> Expr var -> Expr a
expandVars defaultVal list exp = fmap (findWithDefault defaultVal list) exp


data GExpr f a
  = GVar a -- variable
  | GOp (f (GExpr f a)) -- generalised operation/literal

data IExpr expr
  = ILit Integer
  | IAdd expr expr
  | IMul expr expr
  | ISub expr expr
  | IPow expr expr
  deriving (Show, Functor)

-- fromExpr :: Expr a -> GExpr IExpr a
-- fromExpr (Lit n) = ILit n
-- fromExpr (Var x) = GVar x
-- fromExpr (Add e1 e2) = GOp (IAdd e1 e2)


test1 = evalWith 0 [("x", 2), ("y", 3)] (Add (Var "x") (Var "y"))
test2 =   evalWith 0 [("x", 2), ("y", 3)] ( Add (Pow ( Add (Var "x") (Var "y") ) (Lit 2) ) (Var "z") )
test3 = displayWith show (Mul (Var "x") (Add (Lit 2) (Var "y")))


main :: IO ()
main = do
  putStrLn "hello world"
