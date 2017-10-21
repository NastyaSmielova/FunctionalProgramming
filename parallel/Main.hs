module Main where
import Control.Parallel
import Control.Seq

compareEps ::Double -> Double -> Double -> Bool
compareEps a b eps = abs( a - b) < eps

sumPar :: [Double] -> Double
sumPar [x] = x
sumPar [] = 0
sumPar (x:xs) = n1 `par` n2 `seq`(n1+n2)
                             where
                              n1 = sumPar xs
                              n2 =x

find::(Double -> Double)->Double -> Double->  Integer -> Double
find f a b n  =  n1 `par` n2 `seq`
             ((n1 + n2 )*((b-a)/ fromIntegral n))
                      where
                             n1 = sumPar (map ( f) [a, (a + (b-a)/fromIntegral n)..( b/2)]   )
                             n2 = sumPar (map ( f) [( b /2 +(b-a)/fromIntegral n), (b/2 + 2*(b-a)/fromIntegral n).. b ]   )


findNext::Double ->(Double -> Double)->Double -> Double->  Double -> Integer -> Double
findNext next f a b eps n = do
         let newNext = find f a b n
         if (compareEps newNext next eps )then newNext
         else findNext newNext f a b eps ( n * 2)


integ:: (Double -> Double) -> Double -> Double -> Double -> Double
integ f a b eps |(a>=b)=0
                |otherwise = do
                    let cur = find f a b 1000
                    findNext  cur f a b  eps 2000

f x = x+3

main :: IO ()
main = do
  print (integ f 1 10 0.1)