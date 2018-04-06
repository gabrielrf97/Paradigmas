module SimpleGraphs where

import Control.Monad (replicateM)
import System.IO (Handle, hGetLine)
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)

data EdgeSpec = EdgeSpec
  { fromNode :: Int
  , toNode :: Int
  , distance :: Int
  }

readInputs :: Handle -> IO (Int, [EdgeSpec])
readInputs handle = do
  numNodes <- read <$> hGetLine handle
  numEdges <- (read <$> hGetLine handle)
  edges <- replicateM numEdges (readEdge handle)
  return (numNodes, edges)

readEdge :: Handle -> IO EdgeSpec
readEdge handle = do
  input <- hGetLine handle
  let [f_s, t_s, d_s] = words input
  return $ EdgeSpec (read f_s) (read t_s) (read d_s)

newtype NodeLabel = NodeLabel Int 
type Distance = Int

genGraph :: (Int, [EdgeSpec]) -> Gr NodeLabel Distance
genGraph (numNodes, edgeSpecs) = mkGraph nodes edges
  where
    nodes = (\i -> (i, NodeLabel i)) 
      <$> [1..numNodes]
    edges = (\es -> (fromNode es, toNode es, distance es))
      <$> edgeSpecs