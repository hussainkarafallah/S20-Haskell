{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Function (on)

import Data.List (sortBy)
naiveMean :: Fractional a => [a] -> a
naiveMean xs = sum xs / fromIntegral (length xs)

-- mean data type wrapper
data Mean a = Mean {
    count :: Int , 
    mean :: a
  }  
  deriving (Show)


instance Fractional a => Semigroup (Mean a) where
  Mean n x <> Mean 0 _ = Mean n x
  Mean 0 _ <> Mean n x = Mean n x
  Mean n x <> Mean m y = Mean (n + m) (x + (y - x) * (fromIntegral m) / fromIntegral(n + m))
  
instance Fractional a => Monoid (Mean a) where
  mempty = Mean 0 0

-- a function that gets the mean of a list of numbers
getMean :: Fractional a => [a] -> Mean a
getMean = mconcat . map (Mean 1)  

--variance data type wrapper
data Variance a = Variance Int a a
  deriving (Show)

instance Fractional a => Semigroup (Variance a) where
  Variance 0 _ _ <> Variance a b c = Variance a b c
  Variance a b c <> Variance 0 _ _ = Variance a b c
  Variance countX meanX varX <> Variance countY meanY varY = Variance resCount resMean resVar
    where
      resCount = countX + countY
      resMean = mean ( (Mean countX meanX) <> (Mean countY meanY) )
      meanDif = meanY - meanX
      resVar = (varX * fromIntegral(countX - 1) + varY * fromIntegral(countY - 1) + meanDif^2 *  fromIntegral(countX * countY) / (fromIntegral resCount )) / fromIntegral(resCount - 1)

instance Fractional a => Monoid (Variance a) where
  mempty = Variance 0 0 0

-- a function that gets the variance of a list of numbers
variance :: Fractional a => [a] -> Variance a
variance = mconcat . map (\x -> Variance 1 x 0)

-- a type representing intervals characterized by start and end
-- start <= end
data Interval t = Interval {
    start :: t , 
    end :: t
  }
  deriving (Show)

-- interval set type
newtype IntervalSet t = IntervalSet {
    intervals :: [Interval t]
  }
  deriving (Show)

-- intervalset to list of pairs
fromIntervalSet :: IntervalSet t -> [(t, t)]
fromIntervalSet (IntervalSet set) = map (\(Interval a b) -> (a,b)) set

-- list of pairs to intervalset
unsafeIntervalSet :: [(t, t)] -> IntervalSet t
unsafeIntervalSet = IntervalSet . map (\(a,b) -> Interval a b)

-- union function
union :: Ord t => IntervalSet t -> IntervalSet t -> IntervalSet t
union x@(IntervalSet s) (IntervalSet []) = x 
union x@(IntervalSet []) (IntervalSet s) = x 
union (IntervalSet s1) (IntervalSet s2) 
  | end x < start y = IntervalSet (x : y : rest) -- no intersection
  | start x > end y = IntervalSet (y : x : rest) -- no intersection
  | otherwise = IntervalSet (combined : rest) -- merge the head intervals
  where
    x:xs = s1
    y:ys = s2
    combined = Interval (min (start x) (start y)) (max (end x) (end y))
    rest = intervals (IntervalSet xs <> IntervalSet ys)

instance Ord t => Semigroup (IntervalSet t) where
  s1 <> s2 = union s1 s2

instance Ord t => Monoid (IntervalSet t) where
  mempty = IntervalSet []

-- a fucntion that safely constructs an interval from a pair
pair2interval :: Ord t => (t, t) -> Interval t
pair2interval (a, b)
  | a > b = Interval b a
  | otherwise = Interval a b

-- a safe version for list of pairs to intervalset
intervalSet :: Ord t => [(t, t)] -> IntervalSet t
intervalSet = mconcat . map (\x -> IntervalSet [pair2interval x])
  

test1 = getMean [2,7,9]
test2 = getMean [4,3]
test3 = getMean (replicate 100 1e308)
test4 = variance [1,3,5,7,9]
test5 = variance [2,3,4,5,7]


i1 = unsafeIntervalSet [(1, 4), (6, 9)]
i2 = unsafeIntervalSet [(2, 5), (7, 8)]

test6 = fromIntervalSet (i1 `union` i2)
test7 = fromIntervalSet (intervalSet [('a', 'k'), ('A', 'Z'), ('z', 'j')])
test8 = fromIntervalSet ( (intervalSet [(6, 9), (4, 1)]) `union` (intervalSet [(5, 2), (7, 8)]) )

main = do
  putStrLn "hi"