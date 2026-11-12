module Euler18 where

triangle :: [[Int]]
triangle = map (map read . words) (lines raw)

raw :: String
raw = unlines
  [ "75"
  , "95 64"
  , "17 47 82"
  , "18 35 87 10"
  , "20 04 82 47 65"
  , "19 01 23 75 03 34"
  , "88 02 77 73 07 63 67"
  , "99 65 04 28 06 16 70 92"
  , "41 41 26 56 83 40 80 70 33"
  , "41 48 72 33 47 32 37 16 94 29"
  , "53 71 44 65 25 43 91 52 97 51 14"
  , "70 11 33 28 77 73 17 78 39 68 17 57"
  , "91 71 52 38 17 14 91 43 58 50 27 29 48"
  , "63 66 04 68 89 53 67 30 73 16 69 87 40 31"
  , "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
  ]

depth :: Int
depth = length triangle - 1

value :: [Int] -> Int
value p = sum (zipWith (!!) triangle (scanl (+) 0 p))

paths :: [[[Int]]]
paths = iterate (concatMap (\p -> [0 : p, 1 : p])) [[]]

rec :: Int
rec = go 0 0
  where
    go r c
      | r == depth = triangle !! r !! c
      | otherwise = triangle !! r !! c + max (go (r + 1) c) (go (r + 1) (c + 1))

tailRec :: Int
tailRec = go (depth - 1) (last triangle)
  where
    go r acc
      | r < 0 = head acc
      | otherwise = go (r - 1) (zipWith3 (\x a b -> x + max a b) (triangle !! r) acc (tail acc))

mapped :: Int
mapped = head (foldr1 step triangle)
  where
    step r acc = map (\(x, a, b) -> x + max a b) (zip3 r acc (tail acc))

modular :: Int
modular = foldr (max . value) 0 (filter ((== depth) . length) (concat (take (depth + 1) paths)))

comprehension :: Int
comprehension = maximum [value p | p <- sequence (replicate depth [0, 1])]

infinite :: Int
infinite = maximum (map value (paths !! depth))
