module Main where

import           Relude
import           Control.Monad.Loops (iterateUntilM)
import           Data.Text           (splitOn)
import           Data.Vector         ((!), (//), Vector)
import qualified Data.Vector         as V
import           Text.Read           (read)

main :: IO ()
main = do
  input <- readFileText "input/day-5.txt"
  let program :: Program = V.fromList $ map (read . toString) $ splitOn "," input
  void $ run program


output :: Program -> Int
output p = p ! 0


start :: Int -> Int -> Program -> Program
start noun verb p = p // [(1, noun), (2, verb)]


run :: MonadIO m => Program -> m Program
run p = snd <$> iterateUntilM halted step initial
  where
    initial = (Position 0, p)
    halted (p', _) = p' == End


step :: MonadIO m => ProgramState -> m ProgramState
step p@(End, _) = pure p
step (Position pc, v) = do
  let instruction = makeParameterInstruction (v ! pc)
  case opcode instruction of
    1 -> pure (Position (pc+4), v // [(v ! (pc+3), apply (+) instruction (pc+1) (pc+2))])
    2 -> pure (Position (pc+4), v // [(v ! (pc+3), apply (*) instruction (pc+1) (pc+2))])
    3 -> do
      putTextLn "Input: "
      input <- readEither <$> getLine
      case input of
        Left e -> error e
        Right i -> pure (Position (pc+2), v // [(v ! (pc+1), i)])
    4 -> do
      putTextLn $ "Output: " <> show (param (pc+1) (mode1 instruction))
      pure (Position (pc+2), v)
    5 -> if param (pc+1) (mode1 instruction) /= 0
      then pure (Position (param (pc+2) (mode2 instruction)), v)
      else pure (Position (pc+3), v)
    6 -> if param (pc+1) (mode1 instruction) == 0
      then pure (Position (param (pc+2) (mode2 instruction)), v)
      else pure (Position (pc+3), v)
    7 -> if param (pc+1) (mode1 instruction) < param (pc+2) (mode2 instruction)
      then pure (Position (pc+4), v // [(v ! (pc+3), 1)])
      else pure (Position (pc+4), v // [(v ! (pc+3), 0)])
    8 -> if param (pc+1) (mode1 instruction) == param (pc+2) (mode2 instruction)
      then pure (Position (pc+4), v // [(v ! (pc+3), 1)])
      else pure (Position (pc+4), v // [(v ! (pc+3), 0)])
    _ -> pure (End, v)
  where
    apply op i p1 p2 = param p1 (mode1 i) `op` param p2 (mode2 i)
    param x Positional = deref x
    param x Immediate = v ! x
    deref pos = v ! (v ! pos)


makeParameterInstruction :: Int -> ParameterInstruction
makeParameterInstruction i = case leftPad (digits i) of
  [ m2, m1, op1, op2 ] -> ParameterInstruction
    { opcode = op1 * 10 + op2
    , mode1 = toMode m1
    , mode2 = toMode m2 }
  _ -> error $ "Invalid instruction " <> show i
  where
    toMode 0 = Positional
    toMode _ = Immediate
    leftPad xs@[ _ ] = 0 : 0 : 0 : xs
    leftPad xs@[ _, _ ] = 0 : 0 : xs
    leftPad xs@[ _, _, _ ] = 0 : xs
    leftPad xs = xs

digits :: Integral a => a -> [a]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]


type Program = Vector Int
type ProgramState = (ProgramCounter, Program)

data ProgramCounter = Position Int | End deriving (Show, Eq)

data Mode = Immediate | Positional deriving (Show, Eq)

data ParameterInstruction = ParameterInstruction
  { opcode :: Int
  , mode1 :: Mode
  , mode2 :: Mode }
  deriving (Show, Eq)
