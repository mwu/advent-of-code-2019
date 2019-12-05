module Main where


import Relude


main :: IO ()
main = do
  let range :: [[Int]] = map digits [147981..691423]
  putText "Part 1: "
  print $ length $ filter meetsCriteria range

  putText "Part 2: "
  print $ length $ filter meetsTrueCriteria range


meetsTrueCriteria :: (Eq a, Ord a) => [a] -> Bool
meetsTrueCriteria xs = isLengthSix xs && hasAtLeastOnePair xs && isOrdered xs


meetsCriteria :: (Eq a, Ord a) => [a] -> Bool
meetsCriteria xs = isLengthSix xs && hasAPair xs && isOrdered xs


isOrdered :: Ord a => [a] -> Bool
isOrdered xs = sort xs == xs


hasAPair :: Eq a => [a] -> Bool
hasAPair xs = any ((>= 2) . length) $ group xs


hasAtLeastOnePair :: Eq a => [a] -> Bool
hasAtLeastOnePair xs = any ((== 2) . length) $ group xs


isLengthSix :: [a] -> Bool
isLengthSix xs = length xs == 6


digits :: Integral a => a -> [a]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]
