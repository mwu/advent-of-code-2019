module Main where

import Prelude
import Data.List (find)
import Data.Vector ((!), (//), Vector, fromList)


main :: IO ()
main = do
  input <- readFile "input/day-2.txt"
  let program = read $ "[" ++ input ++ "]"

  putStr "Part 1: "
  print $ output $ run $ restoreGAP program

  putStr "Part 2: "
  print $ (\(n,v) -> n * 100 + v) <$> findNounAndVerb 19690720 program


findNounAndVerb :: Int -> Program -> Maybe (Int, Int)
findNounAndVerb expected program = snd <$> find matching candidates
  where
    candidates = do
      noun <- [0..99]
      verb <- [0..99]
      let out = output $ run $ start noun verb program
      pure (out, (noun, verb))
    matching (out, _) = out == expected


output :: Program -> Int
output p = p ! 0


restoreGAP :: Program -> Program
restoreGAP = start 12 2


start :: Int -> Int -> Program -> Program
start noun verb p = p // [(1, noun), (2, verb)]


run :: Program -> Program
run p = snd $ head $ dropWhile running $ iterate step init
  where
    init = (Position 0, p)
    running (p, _) = p /= End


step :: ProgramState -> ProgramState
step p@(End, _) = p
step (Position pc, v) =
  case v ! pc of
    1 -> (Position (pc+4), v // [(v ! (pc+3), (deref (pc+1) v) + (deref (pc+2) v))])
    2 -> (Position (pc+4), v // [(v ! (pc+3), (deref (pc+1) v) * (deref (pc+2) v))])
    _ -> (End, v)
  where
    deref pos v = v ! (v ! pos)


type Program = Vector Int
type ProgramState = (ProgramCounter, Program)
data ProgramCounter = Position Int | End deriving (Show, Eq)
