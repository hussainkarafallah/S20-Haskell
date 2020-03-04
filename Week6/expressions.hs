{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.String

-- | A simple expression with variables.
data Expr a = 
  Lit Integer                -- ^ Integer literal
  | Var a                    -- ^ Variable                   
  | Add (Expr a) (Expr a)    -- ^ Addition. 
  | Mul (Expr a) (Expr a)    -- ^ Multiplication.
  deriving (Show, Eq)
    
instance Functor Expr  where  
  fmap _ (Lit one)              = (Lit one)
  fmap f (Var one)              = Var (f one) 
  fmap f (Add one two)          = Add (fmap f one) (fmap f two)
  fmap f (Mul one two)          = Mul (fmap f one) (fmap f two)

  
-- | Num instance for nice syntax.
instance Num (Expr a) 
  where 
  e1 + e2 = Add e1 e2
  e1 * e2 = Mul e1 e2 
  fromInteger n = Lit n  


evalwithDraft :: Eq var => Expr var ->[(var, Int)]-> Integer-> Integer 
evalwithDraft (Lit n) _ _       = n
evalwithDraft (Var n) x def     
  = toInteger (snd(findTurple (fromIntegral def) (Var n) x))
evalwithDraft (Add e1 e2) x def = evalwithDraft e1 x def + evalwithDraft e2 x def
evalwithDraft (Mul e1 e2) x def = evalwithDraft e1 x def * evalwithDraft e2 x def
  
-- ==============================================  
-- GIVEN FUNCTIONS
-- ==============================================  
-- | Evaluate an expression with all variables instantiated.  
eval :: Expr Integer -> Integer 
eval (Lit n) = n
eval (Var n) = n
eval (Add e1 e2) = eval e1 + eval e2  
eval (Mul e1 e2) = eval e1 * eval e2


-- | Display an expression with variables.
display :: Expr String -> String
display (Lit n) = show n
display (Var s) = s
display (Add e1 e2) = display e1 ++ " + " ++ display e2
display (Mul e1 e2) = "(" ++ display e1 ++ ") * (" ++ display e2 ++ ")"

-- ==============================================  
-- PART 1
-- ==============================================  

-- here we assume that expressions have only var type
-- return is also Var name, Int
findTurple :: Eq var => a ->Expr var -> [(var, a)] -> (Expr var, a)
findTurple defaultVal expr [] = (expr, defaultVal)
findTurple defaultVal expr ((name, intVal):xs)  
  |Var name == expr    = (expr, intVal)
  |otherwise = findTurple defaultVal expr xs
  
  
evalWith:: Eq var => Expr var ->[(var, Int)]-> Integer-> Integer 
evalWith expression maplist def 
  = eval (fmap (\(expr) ->
  toInteger (snd(findTurple (fromIntegral def) (Var expr) maplist))) expression)

--expToStr:: Expr var -> String
--expToStr expression = (fmap (\(expr) ->
 -- toInteger (snd(findTurple (fromIntegral 0) (Var expr) []))) expression)



displayWith :: (var -> String) -> Expr var -> String
displayWith funct expr =  display (fmap (\(expres) -> funct expres) expr )


-- check if it really works
expandVars :: Eq var =>  a ->[(var,  a)]-> Expr(Expr var)->Expr a
expandVars def maplist expression = 
  (fmap (\(expr)->(snd(findTurple def expr maplist ))) expression)

-- ==============================================  
-- SAMPLE WORKINGS
-- ==============================================  


--working Instance for 1.1
a = eval (fmap (\(expr) ->toInteger (snd(findTurple (fromIntegral 0) (Var expr) lst))) expres)  

b = evalWith expres lst 0

mew = Lit 4 
x = Var "x"
y = Var "y"
z = Var "z"
expres = x + 2 * (y + 1)
tosh = (Mul (Var "x") (Add (Lit 2) (Var "y")))
lst = [("x", 1 ), ("y", 2 )]

tr = evalWith expres lst 0

yyy= display (Var "x")

lst2 = [("x", y + z), ("y", x + 3)]

unk = Var "unknown"

expres2 = x*y
mewkmewk = expandVars unk lst2 expres

xy = display tosh 
mur = displayWith show expres
-- ==============================================  
-- MAIN
-- ==============================================  

main :: IO ()
main = putStr (show mur) <> putStr "\n" <> putStr (show a)