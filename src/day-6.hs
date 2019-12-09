module Main where

import           Relude
import           Data.Text                         (splitOn)
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.BFS    as G

import qualified Data.Set                          as Set


main :: IO ()
main = do
  input <- readFileText "input/day-6.txt"
  let orbits = map readOrbit $ lines input
  let objects = Set.fromList $ map orbittee orbits <> map orbitter orbits
  let objectGraph :: G.Gr Object Text = foldr (insertObject objects) G.empty objects
  let orbitterGraph = foldr (insertOrbitter objects) objectGraph orbits
  let orbitGraph = foldr (insertOrbittee objects) orbitterGraph orbits
  
  putText "Part 1: "
  putTextLn
    $ show
    $ sum
    $ map (length . allOrbitsOf orbitterGraph)
    $ G.nodes orbitterGraph

  putText "Part 2: "
  putTextLn
    $ show
    $ subtract 3  -- subtract yourself santa and first edge
    $ length
    $ map (flip Set.elemAt objects)
    $ G.esp (Set.findIndex (Object "YOU") objects) (Set.findIndex (Object "SAN") objects) orbitGraph

  where
    mkObjectNode objects obj = (Set.findIndex obj objects, obj)
    insertObject objects obj graph = G.insNode (mkObjectNode objects obj) graph    
    insertOrbitter objects orbit graph = G.insEdge
      ( Set.findIndex (orbitter orbit) objects
      , Set.findIndex (orbittee orbit) objects
      , label orbit )
      graph
    insertOrbittee objects orbit graph = G.insEdge
      ( Set.findIndex (orbittee orbit) objects
      , Set.findIndex (orbitter orbit) objects
      , label orbit )
      graph


allOrbitsOf :: G.DynGraph gr => gr a b -> G.Node -> [G.Node]
allOrbitsOf graph node =
  case G.suc graph node of
    [] -> []
    os -> foldMap (allOrbitsOf graph) os <> os


newtype Object = Object Text deriving (Show, Eq, Ord)

data Orbit = Orbit
  { orbittee :: Object
  , orbitter :: Object
  , label :: Text }
  deriving (Show, Eq)


readOrbit :: Text -> Orbit
readOrbit input = let [o, r] = splitOn ")" input in
  Orbit { orbittee = Object o, orbitter = Object r, label = input }
