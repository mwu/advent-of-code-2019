module Main where

import Prelude


main :: IO ()
main = do
  input <- readFile "input/day-1.txt"
  let modules = lines input

  putStr "Part 1: "
  print $ sum $ map (fuelForModule . read) modules

  putStr "Part 2: "
  print $ sum $ map (fuelForModuleAndFuel . read) modules
    

fuelForModuleAndFuel :: Integer -> Integer
fuelForModuleAndFuel mass =
  sum $ takeWhile (> 0) $ tail $ iterate fuelForModule mass


fuelForModule :: Integer -> Integer
fuelForModule mass = mass `div` 3 - 2
