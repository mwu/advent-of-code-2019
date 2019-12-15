module Main where

import           Relude
import           Data.List       (minimumBy)
import qualified Data.List.Split as Split
import qualified Data.Text       as T


main :: IO ()
main = do
  input <- readFileText "input/day-8.txt"

  putTextLn "Part 1"
  let inLayers = layers 25 6 input
  putTextLn $ show $ part1 inLayers

  putTextLn "Part 2"
  putTextLn $ prettify $ part2 inLayers
  

part1 :: [Layer] -> Int
part1 ls = numOnes * numTwos
  where
    numOnes = numOf '1' layer
    numTwos = numOf '2' layer
    layer = minZeroLayer ls


part2 :: [Layer] -> [Layer]
part2 ls = map (map visiblePixel . T.transpose) $ transpose ls


prettify :: [Layer] -> Text
prettify ls = T.intercalate "\n" $ map (T.map toBlock . mconcat) ls
  where
    toBlock '1' = '\x2588'
    toBlock _ = ' '

visiblePixel :: Text -> Text
visiblePixel t = case T.dropWhile (== '2') t of
  "" -> "2"
  t' -> T.take 1 t'


minZeroLayer :: [Layer] -> Text
minZeroLayer ls = minimumBy (comparing (numOf '0')) $ map T.concat ls


numOf :: Char -> Text -> Int
numOf c t = T.length $ T.filter (== c) t


layers :: Width -> Height -> Text -> [Layer]
layers w h t = Split.chunksOf h $ T.chunksOf w t


type Layer = [Text]
type Width = Int
type Height = Int
