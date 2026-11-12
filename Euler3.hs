module Euler3 where

import Data.List (foldl')

n :: Integer
n = 600851475143

isqrt :: Integer -> Integer
isqrt = floor . sqrt . (fromIntegral :: Integer -> Double)

isPrime :: Integer -> Bool
isPrime x = x > 1 && all ((/= 0) . mod x) [2 .. isqrt x]

tailRec :: Integer -> Integer
tailRec x = go x 2
  where
    go m d
      | d * d > m = m
      | m `mod` d == 0 = go (m `div` d) d
      | otherwise = go m (d + 1)

rec :: Integer -> Integer
rec x = maximum (go x 2)
  where
    go m d
      | d * d > m = [m]
      | m `mod` d == 0 = d : go (m `div` d) d
      | otherwise = go m (d + 1)

divisors :: Integer -> [Integer]
divisors x =
  concat [if d * d == x then [d] else [d, x `div` d]
         | d <- [1 .. isqrt x], x `mod` d == 0]

modular :: Integer -> Integer
modular = foldl' max 1 . filter isPrime . divisors

mapped :: Integer -> Integer
mapped x = maximum (filter isPrime (ds ++ map (x `div`) ds))
  where
    ds = [d | d <- [1 .. isqrt x], x `mod` d == 0]

comprehension :: Integer -> Integer
comprehension x =
  maximum [p | d <- [1 .. isqrt x], x `mod` d == 0, p <- [d, x `div` d], isPrime p]

primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

infinite :: Integer -> Integer
infinite x = go x primes
  where
    go m ps@(p : rest)
      | p * p > m = m
      | m `mod` p == 0 = go (m `div` p) ps
      | otherwise = go m rest
    go m [] = m
