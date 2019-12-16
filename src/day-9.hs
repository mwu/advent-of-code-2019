module Main where

import           Relude
import           Control.Monad.Loops      (iterateUntilM)
import           Data.Text                (splitOn)
import qualified Data.Map.Strict          as M
import           Text.Read                (read)


main :: IO ()
main = do
  input <- readFileText "input/day-9.txt"

  let program = M.fromList $ zip [0..] $ map (read . toString) $ splitOn "," input
  run program


run :: MonadIO m => Program -> m ()
run p = do
  void $ iterateUntilM halted step initial
  where
    initial = ProgramState
      { _pc = Position 0
      , _program = p
      , _relativeBase = 0 }
    halted p' = _pc p' == End


step :: MonadIO m => ProgramState -> m ProgramState
step p@(ProgramState { _pc = End }) = pure p
step p@(ProgramState { _pc = Position pc
                     , _program = v
                     , _relativeBase = relBase }) = do
  let instruction = makeParameterInstruction (v M.! pc)

  case opcode instruction of
    Add ->
      pure $ p { _pc = Position (pc+4)
               , _program = v // ( location3 instruction
                                 , param1 instruction + param2 instruction )
               }
    Multiply ->
      pure $ p { _pc = Position (pc+4)
               , _program = v // ( location3 instruction
                                 , param1 instruction * param2 instruction )
               }
    Input -> do
      putTextLn "Input: "
      input <- readEither <$> getLine
      case input of
        Left e -> error e
        Right i -> pure $ p { _pc = Position (pc+2)
                            , _program = v // (location1 instruction, i) }
    Output -> do
      putTextLn $ "Output: " <> show (param1 instruction)
      pure $ p { _pc = Position (pc+2) }
    JumpIfTrue ->
      if param1 instruction /= 0
      then pure $ p { _pc = Position (param2 instruction) }
      else pure $ p { _pc = Position (pc+3) }
    JumpIfFalse ->
      if param1 instruction == 0
      then pure $ p { _pc = Position (param2 instruction) }
      else pure $ p { _pc = Position (pc+3) }
    LessThan ->
      pure $ p { _pc = Position (pc+4)
               , _program = v // ( location3 instruction 
                                 , if param1 instruction < param2 instruction
                                   then 1
                                   else 0 )
               }
    Equals ->
      pure $ p { _pc = Position (pc+4)
               , _program = v // ( location3 instruction
                                 , if param1 instruction == param2 instruction
                                   then 1
                                   else 0 )
               }
    OffsetRelativeBase ->
      pure $ p { _pc = Position (pc+2)
               , _relativeBase = relBase + (param1 instruction)
               }
    _ -> pure $ p { _pc = End }
  where
    (//) m (k, x) = M.insert k x m
    (!) m k = fromMaybe 0 (m M.!? k)

    location x Positional = v ! x
    location x Immediate = x
    location x Relative = relBase + (v ! x)

    param x mode = v ! (location x mode)

    location1 instruction = location (pc+1) (mode1 instruction)
    location3 instruction = location (pc+3) (mode3 instruction)
    
    param1 instruction = param (pc+1) (mode1 instruction)
    param2 instruction = param (pc+2) (mode2 instruction)


makeParameterInstruction :: Integer -> ParameterInstruction
makeParameterInstruction i = case leftPad (digits i) of
  [ m3, m2, m1, op1, op2 ] -> ParameterInstruction
    { opcode = toOpCode (op1 * 10 + op2)
    , mode1 = toMode m1
    , mode2 = toMode m2
    , mode3 = toMode m3 }
  _ -> error $ "Invalid instruction " <> show i
  where
    toMode 0 = Positional
    toMode 1 = Immediate
    toMode 2 = Relative
    toMode x = error $ "Invalid mode " <> show x

    toOpCode 1 = Add
    toOpCode 2 = Multiply
    toOpCode 3 = Input
    toOpCode 4 = Output
    toOpCode 5 = JumpIfTrue
    toOpCode 6 = JumpIfFalse
    toOpCode 7 = LessThan
    toOpCode 8 = Equals
    toOpCode 9 = OffsetRelativeBase
    toOpCode _ = Halt

    leftPad xs = replicate (5 - length xs) 0 <> xs


digits :: Integral a => a -> [a]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]


type Program = Map Integer Integer

data ProgramState = ProgramState
  { _pc :: ProgramCounter
  , _program :: Program
  , _relativeBase :: Integer }
  deriving (Show, Eq)

data ProgramCounter = Position Integer | End deriving (Show, Eq)

data OpCode
  = Add
  | Multiply
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | OffsetRelativeBase
  | Halt
  deriving (Show, Eq)

data Mode = Immediate | Positional | Relative deriving (Show, Eq)

data ParameterInstruction = ParameterInstruction
  { opcode :: OpCode
  , mode1 :: Mode
  , mode2 :: Mode
  , mode3 :: Mode }
  deriving (Show, Eq)
