module Main where


import           Relude
import           Data.Text (splitOn)
import qualified Data.Set  as Set
import           Text.Read (Read(..), read)


main :: IO ()
main = do
  input <- readFileText "input/day-3.txt"
  let [line1, line2] = lines input

  let wire1 = map (read . toString) $ splitOn "," line1
  let wire2 = map (read . toString) $ splitOn "," line2

  let centralPort = (0,0)
  let wire1Points = points centralPort wire1
  let wire2Points = points centralPort wire2

  putText "Part 1: "
  let crossings = Set.fromList wire1Points `Set.intersection` Set.fromList wire2Points
  let distances = Set.map (distance centralPort) crossings
  putTextLn $ show $ Set.elemAt 0 distances

  putText "Part 2: "
  let distancesToCrossings1 = distancesBetween wire1Points crossings
  let distancesToCrossings2 = distancesBetween wire2Points crossings
  let totalDistancesToCrossings = sort $ map (uncurry (+)) $ zip distancesToCrossings1 distancesToCrossings2
  putTextLn $ show $ head <$> nonEmpty totalDistancesToCrossings


distancesBetween :: [Point] -> Set.Set Point -> [Integer]
distancesBetween ps destinations = map fst
  $ sortOn snd
  $ filter (\(_, x) -> Set.member x destinations)
  $ zip [1..] ps


distance :: Point -> Point -> Integer
distance (x1, y1) (x2, y2) = (abs $ x2 - x1) + (abs $ y2 - y1)


points :: Point -> [Move] -> [Point]
points centralPort moves = snd $ foldl' pointsBetween (centralPort, []) moves
  where
    pointsBetween (start, pts) move = (moveFrom start move, pts <> trail start move)


moveFrom :: Point -> Move -> Point
moveFrom (x, y) (Move U d) = (x, y+d)
moveFrom (x, y) (Move R d) = (x+d, y)
moveFrom (x, y) (Move D d) = (x, y-d)
moveFrom (x, y) (Move L d) = (x-d, y)


-- points in a move excluding the starting point
trail :: Point -> Move -> [Point]
trail (x, y) (Move direction d) = drop 1 $ case direction of
                                    U -> [ (x, y') | y' <- [y..y+d] ]
                                    R -> [ (x', y) | x' <- [x..x+d] ]
                                    D -> [ (x, y') | y' <- reverse [y-d..y] ]
                                    L -> [ (x', y) | x' <- reverse [x-d..x] ]


data Move = Move Direction Integer deriving (Show, Eq)

instance Read Move where
  readsPrec _ input = case input of
                        'U' : xs -> [(Move U (read xs), "")]
                        'R' : xs -> [(Move R (read xs), "")]
                        'D' : xs -> [(Move D (read xs), "")]
                        'L' : xs -> [(Move L (read xs), "")]
                        _        -> []


data Direction = U | R | D | L deriving (Show, Eq)

type Point = (Integer, Integer)
