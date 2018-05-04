module Lib
    ( someFunc
    , newProblem
    , allStates
    , isPathPossible
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe

data Jug = Jug Int Int deriving (Show, Eq, Ord)

data State = State Jug Jug deriving (Show, Eq, Ord)

data Problem = Problem State State deriving (Show, Eq, Ord)

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
        m' = M.insert current ns m
        queue' = S.delete current queue
        new_q = S.fromList $ filter (\s -> M.notMember s m) ns
        queue'' = if S.member f new_q
          then queue'
          else S.union queue' new_q
        next = head $ S.toList queue''
  in
    allStates' i (S.fromList [i]) M.empty

isPathPossible :: Problem -> StateMap -> Bool
isPathPossible (Problem _ f) m = S.member f $ S.fromList $ concat $ M.elems m

someFunc :: IO ()
someFunc = putStrLn "someFunc"
