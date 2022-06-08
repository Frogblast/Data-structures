module Graph where
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.PSQueue as PQ
import Data.Maybe ( fromMaybe )

data Graph a b = Graph
  { adjMap :: M.Map a [Edge a b]
  }
  deriving (Show)

data Edge a b = Edge
  { source :: a,
    cost :: b,
    destination :: a
  }
 deriving (Show, Eq, Ord)


----- GRAPH FUNCTIONS ----

emptyGraph :: Graph a Integer
emptyGraph = Graph M.empty

-- Returns all edges which has 'a' as its source node in the graph. If no adjacent node is found, it returns an empty list.
getNeighbours :: (Ord a, Ord b) => Graph a b -> a -> [Edge a b]
getNeighbours g a = M.findWithDefault [] a (adjMap g)

-- Adds a single edge to a graph
addEdge :: (Ord a, Ord b) => Graph a b -> Edge a b -> Graph a b
addEdge g edge = Graph {adjMap = M.insertWith (++) (source edge) [edge] (adjMap g)}

-- Recursively adds several nodes to a graph
addEdges :: (Ord a, Ord b) => Graph a b -> [Edge a b] -> Graph a b
addEdges g [] = g
addEdges g [e] = addEdge g e
addEdges g (e:es) = addEdges (addEdge g e) es

---- DIJKSTRA STUFF ----
-- Finds all reachable nodes from the source node in a graph, where each path has the lowest cost
dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> a -> M.Map a (a, b)
dijkstra g startNode = dijkstraRecursive g startingPQ M.empty 
  where startingPQ = PQ.singleton (Edge startNode 0 startNode) 0

{- 
Takes in a graph, a priorityqueue and a map in which we map each node, from where it was reached and what the cost was.
The process is:
1. If the priority queue (PQ) is empty, it means we have no more reachable nodes in the graph to explore and we are done.
2. If there is an edge to some node in the PQ, which we have already mapped in the result map we simply remove the edge from the PQ
    This means we already have the shortest path there, since the edge that took us there has to have a lower cost (was in PQ before)
3. If there is an edge in the PQ, which has not been mapped in the result map, we have to put it there.
    We map the destination node of the edge, together with it's previous node (source of the edge) and the accumulated cost to get there
-}
dijkstraRecursive :: (Ord a, Ord b, Num b) => Graph a b -> PQ.PSQ (Edge a b) b -> M.Map a (a, b) -> M.Map a (a, b)
dijkstraRecursive g pq result = case PQ.findMin pq of 
    Nothing -> result
    Just edge -> if M.member (destination currentEdge) result
        then dijkstraRecursive g (PQ.delete currentEdge pq) result
        else dijkstraRecursive g updatedPQ updatedMap 
        where
            currentEdge = PQ.key edge
            updatedMap  = M.insert (destination currentEdge) (source currentEdge, cost currentEdge) result
            updatedPQ   = addToPQ (cost currentEdge) (getNeighbours g (destination currentEdge)) pq


-- Adds all adjacent nodes (outgoing edges) into the PQ, but with updated costs.
  -- The updated cost for the edges is the cost of reaching the destination node plus the cost for each of the outgoing edges.
    -- This way we ensure we will always take the cheapest reachable edge in the Dijkstra algorithm
addToPQ :: (Ord a, Ord b, Num b) => b -> [Edge a b] -> PQ.PSQ (Edge a b) b -> PQ.PSQ (Edge a b) b
addToPQ _ [] pq = pq
addToPQ b (e : es) pq = addToPQ b es updatedPQ
 where
   updatedPQ = PQ.insert newEdge (cost newEdge) pq
   newEdge = updateEdgeCost b e

-- Takes in the cost of getting to the node and adds it to the cost of an outgoing edge from this node
updateEdgeCost :: (Num b) => b -> Edge a b -> Edge a b
updateEdgeCost b (Edge s c d) = Edge s (c+b) d