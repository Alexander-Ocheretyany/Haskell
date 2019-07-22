-- Alexander Ocheretyany, 2019
-- Dijkstra's Algorithm 
-- -- -- -- -- -- -- -- -- -- --
-- Input: source node, destination node, list of edges and costs
-- Node: (1st end vertex, 2nd end vertex, non-negative distance)

-- EXAMPLE:
-- Query: dijkstra 0 4 [(0,1,4),(0,7,8),(1,7,11),(1,2,8),(7,8,7),(7,6,1),(2,3,7),(2,8,2),(2,5,4),(8,6,6),(6,5,2),(3,5,14),(3,4,9),(5,4,10)]
-- Output: Just [0,7,6,5,4]

import Data.List

-- Get the indices of vertices
get_indices xs = (sort . nub . concat) [[a, b] | (a, b, _) <- xs]

-- Dijkstra's algorithm
dijkstra s t gs = let indices = get_indices gs in
                  algorithm s t [([s],0)] gs []

-- The actual algorithm
algorithm _ _ [] _ _ = Nothing
algorithm source target (((a:cs), b):qs) gs visited = if a == target then Just (reverse (a:cs))
                                                      else let neighbors = find_neighbors a gs [] in
                                                      let uqs = step ((a:cs),b) qs gs visited neighbors in
                                                      algorithm source target uqs gs (a:visited)

-- Find the distance of a node in the queue
find_dist index xs = let k = [d | ((p:_), d) <- xs, p == index] in
                     if k == [] then Nothing
                     else Just (head k)

-- Find the neighbors of a node
find_neighbors index [] acc = acc
find_neighbors index ((a,b,c):gs) acc | a == index = find_neighbors index gs ((b,c):acc)
                                      | b == index = find_neighbors index gs ((a,c):acc)
                                      | otherwise = find_neighbors index gs acc

-- Make a step of the algorithm
step _ qs _ _ [] = qs
step ((u:us),b) qs gs visited ((v,d):ns) = let uqs = if not (elem v visited) && all (\nd -> b + d < nd) (find_dist v qs) then let alt = b + d in put_into_queue ((v:u:us), alt) qs
                                                     else qs
                                           in step ((u:us),b) uqs gs visited ns

-- Put the element el into the queue h:qs according to its distance
put_into_queue el q = insertBy (\ (_, d) (_, e) -> compare d e) el q