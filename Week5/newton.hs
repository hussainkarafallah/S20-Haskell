{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
import CodeWorld
import Data.Text
import Data.Maybe

root 
  :: Fractional a
  => Int -- ^ Maximum number of iterations
  -> (a -> Bool) -- ^ An acceptable error (precision)
  -> (a -> a) -- ^ Function f to find zero for
  -> (a -> a) -- ^ Derivative f' of function f
  -> a -- ^ Initial guess
  -> Maybe (Int, a) -- ^ Number of iterations and root (where function is zero), if found

-- the step function for newton raphson method
getNextRoot :: Fractional a => (a->a) -> (a->a) -> a -> a
getNextRoot f df curRoot  = curRoot - ((f curRoot) / (df curRoot))

-- the function to validate the result which takes a tuple and checks its iterations
validateResult :: Fractional a => Maybe (Int, a) -> Int -> Bool
validateResult Nothing _ = False
validateResult (Just(iter,_)) maxIter = (iter <= maxIter)

-- the required function
-- what's gonna happen is, i will be generating infinite list of roots for our function
-- and keep taking from the list until i find a root or exhaust max iterations
-- i will generate the list with "iterate" and zip it with [0...] for indexing purposes
root maxIter isAccepted f df curRoot
  -- the result here would be either some random number after finishing iterations or a correct root
  -- so we need to validate iterations
  | (validateResult result maxIter) = result
  | otherwise = Nothing
  where
    result = listToMaybe cuttedList 
    -- take the head of the remaining part of roots list
    cuttedList = Prelude.dropWhile (\x -> not (acceptableError (snd x))  && fst(x) <= maxIter) newtonRoots
    -- we keep cutting the full list until we finish iterations or we find suitable error
    acceptableError x = isAccepted  (abs (f x))
    -- a simple function to check the error
    newtonRoots = Prelude.zip [0..] ( iterate (getNextRoot f df) curRoot )
    -- here we have the infinite list construction
    
-- | Get text of Maybe pair
showAnswer :: Show a => Maybe (Int, a) -> Text
showAnswer Nothing = "No answer"
showAnswer (Just (iter, x)) = pack ("root is " ++ (show x) ++ " found after " ++ (show iter)  ++ " iterations")


-- x^2 - 2
f1 :: Maybe (Int, Double)
f1 = root 100 (< 1e-7) (\x -> x^2 - 2) (\x -> 2*x) 123


-- cos x
f2 :: Maybe (Int, Double)
f2 = root 100 (< 1e-12) cos (negate . sin) 1.0


-- x^2 + 100
f3 :: Maybe (Int, Double)
f3 = root 100 (< 1e-12) cos (negate . sin) 0



-- | Main function
main :: IO()
main = drawingOf (lettering (showAnswer f3))
    
   