module Main where

import           Relude
import           Control.Concurrent.Async (mapConcurrently_)
import           Control.Concurrent.Chan  (Chan, newChan, readChan, writeChan)
import           Control.Monad.Loops      (iterateUntilM)
import           Data.List                (maximum)
import           Data.Text                (splitOn)
import           Data.Vector              ((!), (//), Vector)
import qualified Data.Vector              as V
import           Text.Read                (read)


main :: IO ()
main = do
  input <- readFileText "input/day-7.txt"

  let program = V.fromList $ map (read . toString) $ splitOn "," input

  putStrLn "Part 1"
  let phaseSeqs = permutations [0..4]
  maxSignal <- maximum <$> mapM (thruster program) phaseSeqs
  putStrLn $ show maxSignal

  putStrLn "Part 2"
  let loopSeqs = permutations [5..9]
  maxFSignal <- maximum <$> mapM (feedbackLoop program) loopSeqs
  putStrLn $ show maxFSignal


feedbackLoop :: MonadIO m => Program -> [Int] -> m Int
feedbackLoop program phaseSeq = liftIO $ do
  chans@(c0 : cs) <- forM phaseSeq $ \p -> do
    c <- newChan
    writeChan c p
    pure c

  writeChan c0 0
  let pipes = zip chans (cs <> [c0])
  mapConcurrently_ (run program) pipes
  readChan c0


thruster :: MonadIO m => Program -> [Int] -> m Int
thruster program phaseSeq = liftIO $ do
  chans@(c0 : cs) <- forM phaseSeq $ \p -> do
    c <- newChan
    writeChan c p
    pure c

  out <- newChan
  let pipes = zip chans (cs <> [out])
  
  writeChan c0 0
  mapConcurrently_ (run program) pipes
  readChan out


run :: MonadIO m => Program -> (Chan Int, Chan Int) -> m ()
run p cio = do
  void $ iterateUntilM halted (step cio) initial
  where
    initial = (Position 0, p)
    halted (p', _) = p' == End


step :: MonadIO m => (Chan Int, Chan Int) -> ProgramState -> m ProgramState
step _ p@(End, _) = pure p
step (cIn, cOut) (Position pc, v) = do
  let instruction = makeParameterInstruction (v ! pc)
  case opcode instruction of
    1 -> pure (Position (pc+4), v // [(v ! (pc+3), apply (+) instruction (pc+1) (pc+2))])
    2 -> pure (Position (pc+4), v // [(v ! (pc+3), apply (*) instruction (pc+1) (pc+2))])
    3 -> do
      i <- liftIO $ readChan cIn
      pure (Position (pc+2), v // [(v ! (pc+1), i)])
    4 -> do
      liftIO $ writeChan cOut $ param (pc+1) (mode1 instruction)
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
