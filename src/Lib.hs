module Lib
    ( newProblem
    , allStates
    , isPathPossible
    , findPaths
    , shortestPath
    , solve
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- import qualified Data.List as L
import Data.Maybe

--  Jug capacity, holding
data Jug = Jug Int Int deriving (Show, Eq, Ord)

-- State left right
data State = State Jug Jug deriving (Show, Eq, Ord)

-- Problem initial state, destination state
data Problem = Problem State State deriving (Show, Eq, Ord)

-- StateMap, type alias
type StateMap = M.Map State [State]

emptyJug :: Int -> Jug
emptyJug c = Jug c 0

newProblem :: Int -> Int -> Int -> Int -> Problem
newProblem rc lc r l = Problem (State (emptyJug rc) (emptyJug lc)) (State (Jug rc r) (Jug lc l))

forRightFull :: State -> Maybe State
forRightFull (State (Jug rc rh) lj)
  | rc <= rh = Nothing
  | otherwise = Just $ State (Jug rc rc) lj

forRightToLeft :: State -> Maybe State
forRightToLeft (State (Jug rc rh) (Jug lc lh))
  | rh == 0 || lc <= lh = Nothing
  | otherwise = Just $ State (Jug rc (rh -liquidToTransfer)) (Jug lc (lh + liquidToTransfer))
    where
      liquidToTransfer = if maxCanPour >= rh then rh else maxCanPour
      maxCanPour = lc - lh


forRightToEmpty :: State -> Maybe State
forRightToEmpty (State (Jug rc rh) lj)
  | rh == 0 = Nothing
  | otherwise = Just $ State (emptyJug rc) lj

interchange :: (State -> Maybe State) -> State -> Maybe State
interchange f (State rj lj) =
  case f (State lj rj) of
    Nothing -> Nothing
    Just (State rj' lj') -> Just (State lj' rj')

forLeftFull :: State -> Maybe State
forLeftFull = interchange forRightFull

forLeftToRight :: State -> Maybe State
forLeftToRight = interchange forRightToLeft

forLeftToEmpty :: State -> Maybe State
forLeftToEmpty = interchange forRightToEmpty


getNextState :: State -> [State]
getNextState s = catMaybes $ map (\f -> f s) toNextStates
  where
    toNextStates = [forRightToLeft, forRightToEmpty, forLeftFull,
                    forLeftToRight, forLeftToEmpty, forRightFull]

allStates :: Problem -> StateMap
allStates (Problem i f) =
  let
    allStates' :: State -> S.Set State -> StateMap -> StateMap
    allStates' current queue m
      | S.null queue = m
      | otherwise = allStates' next queue'' m'
      where
        ns = getNextState current
        -- insert current state and its possible transitions to the StateMap
        m' = M.insert current ns m
        queue' = S.delete current queue
        -- filter out all the states that have already been visited.
        new_q = S.fromList $ filter (\s -> M.notMember s m) ns
        -- if final state is in one of these, we dont care for other transitions
        -- from that point
        queue'' = if S.member f new_q
          then queue'
          else S.union queue' new_q
        next = head $ S.toList queue''
  in
    allStates' i (S.fromList [i]) M.empty

isPathPossible :: Problem -> StateMap -> Bool
isPathPossible (Problem _ f) m = S.member f $ S.fromList $ concat $ M.elems m

{-
given a problem and its statemaps, find possible non cyclic paths
to the final state from inital state.
You have to ensure that there is a path to final state before calling this.
-}
findPaths :: Problem -> StateMap -> [[State]]
findPaths (Problem i f) m =
  findPaths' [i] []
  where
    findPaths' :: [State] -> [[State]] -> [[State]]
    findPaths' (x:xs) ps
      -- we have found a cycle, return the routes we have found and end the branch
      | x `elem` xs = ps
      -- found a full path, add that to routes and end branch
      | x == f  = (reverse $ x:xs):ps
      | otherwise = case M.lookup x m of
                      Nothing -> ps
                      Just ls -> foldl (\ps' l-> findPaths' (l:x:xs) ps') ps ls

shortestPath :: [[State]] -> [State]
shortestPath ps = foldl (\a x -> if (length a) > (length x)
                          then x else a) (head ps) (tail ps)

solve :: Problem -> Maybe [State]
solve p = if isPathPossible p ss
          then Just $ shortestPath $ findPaths p ss
          else Nothing
  where
    ss = allStates p
