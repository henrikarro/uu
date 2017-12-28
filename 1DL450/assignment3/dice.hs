import Control.Monad (forM)
import Control.Monad.State (State, state, evalState)
import Data.Array ((!))
import Data.Graph (Graph, buildG)
import Data.List (minimumBy, maximumBy, nub)
import Data.Ord (comparing)

import Test.HUnit (Test(..), assertEqual, runTestTT)

-- | A DieRoll is the result of rolling a die, i.e. 1..6.
data DieRoll = DieRoll Int deriving (Eq)

-- | A DieNumber is an index into a list of dice, 0...
type DieNumber = Int

-- | DiceInformation keeps track of available dice and current die.
data DiceInformation = DiceInformation {
  currentDieNumber :: DieNumber,
  dieRolls :: [DieRoll]
  }

-- | A GameNode is an integer 1...
type GameNode = Int

-- | PositionSeen keeps track of one step in the search, to be able to break cycles.
data PositionSeen = PositionSeen {
  dieNumber :: DieNumber,
  nodes :: [GameNode]
  } deriving (Eq)

-- | GameState is information about dice, and a list of positions seen during the search.
data GameState = GameState {
  diceInformation :: DiceInformation,
  positionsSeen :: [PositionSeen]
  }


----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO [()]
main = do
  [numTestCases] <- readInts
  results <- forM [1..numTestCases] (\n -> do
                                        [winningNode, numEdges, numDice] <- readInts
                                        nodes <- readInts
                                        dieRolls <- readInts
                                        return (dice winningNode (pairs nodes) dieRolls))
  mapM print results

-- | reaadInts reads a number of integers from the current line of stdin.
readInts :: IO [Int]
readInts = fmap (map read.words) getLine

-- | pairs creates a list of pair by pairing elements of a list two and two.
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (n1:n2:ns) = (n1,n2):(pairs ns)


--------------------------------------------------------------------------------
-- dice
--------------------------------------------------------------------------------

dice :: GameNode -> [(GameNode, GameNode)] -> [Int] -> Int
dice winningNode edges dieRolls = dice' graph winningNode [1] 0 gameState
  where graph = buildGraph edges
        dieRolls' = map (\d -> DieRoll d) dieRolls
        gameState = GameState{diceInformation=DiceInformation{currentDieNumber=0, dieRolls=dieRolls'}, positionsSeen=[]}

dice' :: Graph -> GameNode -> [GameNode] -> Int -> GameState -> Int
dice' _graph _winningNode [] _moveNumber _gameState = -1
dice' graph winningNode startNodes moveNumber gameState =
  if any (\n -> n == winningNode) startNodes
  then moveNumber
  else if hasSeenPositionBefore gameState (getCurrentDieNumber gameState) startNodes || getCurrentDie gameState == Nothing
       then -1
       else
         let Just (DieRoll d) = getCurrentDie gameState
             gameState' = updateGameStateWithPositionSeen gameState (getCurrentDieNumber gameState) startNodes
             gameState'' = updateGameStateToNextDie gameState'
             endNodes = traverseToDepth graph startNodes d
         in
           dice' graph winningNode endNodes (moveNumber + 1) gameState''

-- | 'traverseToDepth graph startNodes depth' gives the nodes that can be reached by traversing graph depth steops starting from the nodes in startNodes.
traverseToDepth :: Graph -> [GameNode] -> Int -> [GameNode]
traverseToDepth _graph [] _depth = []
traverseToDepth graph startNodes 0 = startNodes
traverseToDepth graph (startNode:startNodes) depth = nub (n1 ++ n2)
  where n1 = traverseToDepth graph (neighbors graph startNode) (depth - 1)
        n2 = traverseToDepth graph startNodes depth
  
buildGraph edges =   buildG (minNode, maxNode) edges
  where minNode = (uncurry min) (minimumBy (comparing (uncurry min)) edges)
        maxNode = (uncurry max) (maximumBy (comparing (uncurry max)) edges)
  
neighbors graph vertex = graph ! (vertex)

--------------------------------------------------------------------------------
-- Functions on the GameState
--------------------------------------------------------------------------------

getCurrentDieNumber :: GameState -> DieNumber
getCurrentDieNumber GameState{diceInformation=DiceInformation{currentDieNumber=n}} = n

getCurrentDie :: GameState -> Maybe DieRoll
getCurrentDie GameState{diceInformation=DiceInformation{currentDieNumber=n, dieRolls=d}} =
  if length d == 0 then Nothing else Just (d !! n)

updateGameStateToNextDie :: GameState -> GameState
updateGameStateToNextDie gameState@GameState{diceInformation=DiceInformation{currentDieNumber=n, dieRolls=d}} = gameState'
  where gameState' = gameState {diceInformation=DiceInformation{currentDieNumber=n', dieRolls=d}}
        n' = if n < length d - 1 then n + 1 else 0

hasSeenPositionBefore :: GameState -> DieNumber -> [GameNode] -> Bool
hasSeenPositionBefore GameState{positionsSeen=p} d n = elem PositionSeen{dieNumber=d, nodes=n} p

updateGameStateWithPositionSeen :: GameState -> DieNumber -> [GameNode] -> GameState
updateGameStateWithPositionSeen gameState@GameState{positionsSeen=p} d n = gameState'
  where gameState' = gameState {positionsSeen=PositionSeen{dieNumber=d, nodes=n}:p}

--------------------------------------------------------------------------------
-- An alternative version of dice that uses the State monad to hold GameState
--------------------------------------------------------------------------------

stDice :: GameNode -> [(GameNode, GameNode)] -> [Int] -> Int
stDice winningNode edges dieRolls = evalState (stDice' graph winningNode [1] 0) gameState
  where graph = buildGraph edges
        dieRolls' = map (\d -> DieRoll d) dieRolls
        gameState = GameState{diceInformation=DiceInformation{currentDieNumber=0, dieRolls=dieRolls'}, positionsSeen=[]}

stDice' :: Graph -> GameNode -> [GameNode] -> Int -> State GameState Int
stDice' _graph _winningNode [] _moveNumber = do return (-1)
stDice' graph winningNode startNodes moveNumber = do
  if any (\n -> n == winningNode) startNodes
    then return moveNumber
    else do
    currentDieNumber <- stGetCurrentDieNumber
    currentDie <- stGetCurrentDie
    positionSeen <- stHasSeenPositionBefore currentDieNumber startNodes
    if positionSeen || currentDie == Nothing
      then return (-1)
      else do
      stUpdateGameStateWithPositionSeen currentDieNumber startNodes
      stUpdateGameStateToNextDie
      let Just (DieRoll d) = currentDie
          endNodes = traverseToDepth graph startNodes d
        in stDice' graph winningNode endNodes (moveNumber + 1)

stGetCurrentDieNumber :: State GameState DieNumber
stGetCurrentDieNumber = state $ \s -> (getCurrentDieNumber s, s)

stGetCurrentDie :: State GameState (Maybe DieRoll)
stGetCurrentDie = state $ \s -> (getCurrentDie s, s)

stUpdateGameStateToNextDie :: State GameState ()
stUpdateGameStateToNextDie = state $ \s -> ((), updateGameStateToNextDie s)

stHasSeenPositionBefore :: DieNumber -> [GameNode] -> State GameState Bool
stHasSeenPositionBefore d n =  state $ \s -> (hasSeenPositionBefore s d n, s)

stUpdateGameStateWithPositionSeen :: DieNumber -> [GameNode] -> State GameState ()
stUpdateGameStateWithPositionSeen d n = state $ \s -> ((), updateGameStateWithPositionSeen s d n)

--------------------------------------------------------------------------------
-- HUnit Test Cases
--------------------------------------------------------------------------------

diceTestCase (expected, winningNode, edges, dieRolls) =
  TestCase (assertEqual (show expected ++ " " ++ show winningNode)
            expected (dice winningNode edges dieRolls))

diceTestData :: [(Int, GameNode, [(GameNode, GameNode)], [Int])]
diceTestData = [
  -- My test cases
  (0, 1, [(1,2)], []),
  (-1, 2, [(1,2)], []),
  (1, 2, [(1,2)], [1]),
  (-1, 2, [(1,2)], [2]),
  (-1, 2, [(1,2),(2,1)], [2]),
  (1, 2, [(1,2),(2,1)], [3]),
  -- Assignment example test cases
  (2, 3, [(1,2),(2,1),(2,3),(3,2)], [3,5]),
  (3, 4, [(1,2),(2,3),(3,4)], [1]),
  (-1, 3, [(1,2),(2,1)], [3]),
  -- Test cases from grading of assignment 2 (except the very long ones)
  (2, 3, [(1,2),(2,1),(2,3),(3,2)], [3,5]),
  (-1, 3, [(1,2),(2,3)], [4,2,6]),
  (12, 10, [(7,4),(1,8),(8,5),(4,1),(2,9),(5,2),(9,6),(3,10),(6,3),(10,7)], [1,1,1,6]),
  (54, 11, [(4,11),(5,1),(9,5),(1,8),(6,2),(10,6),(2,9),(11,7),(7,3),(3,10),(8,4)], [6,5,4,1,6,1,5,5,1,5,6,5]),
  (42, 13, [(7,2),(11,6),(2,10),(8,3),(12,7),(3,11),(13,8),(9,4),(4,12),(10,5),(6,1),(5,13),(1,9)], [3,6,5,1,3,6,1,2,1,4,4,2,5,2,3,5]),
  (42, 14, [(7,2),(11,6),(4,13),(12,7),(8,3),(1,10),(5,14),(13,8),(9,4),(2,11),(6,1),(10,5),(14,9),(3,12)], [5,2,5,6,2,5,1,6,6,1,1,4]),
  (80, 14, [(11,6),(7,2),(4,13),(8,3),(12,7),(5,14),(1,10),(13,8),(9,4),(2,11),(10,5),(14,9),(6,1),(3,12)], [6,4,3,3,4,2,2,6,4,3,3,6,5,1]),
  (-1, 20, [(5,17),(10,2),(14,6),(7,20),(20,13),(3,16),(8,1),(12,5),(16,9),(11,3),(19,11),(6,18),(17,10),(13,6),(3,15),(16,8),(20,12),(7,19),(12,4),(18,11),(10,3),(5,18),(1,14),(4,16),(17,9),(13,5),(11,4),(2,15),(6,19),(15,8)], [2,4,4,1,5,5,4,5,3,4,1,5,2,3,2,1,4,6,6]),
  (-1, 20, [(19,5),(14,20),(10,16),(12,19),(8,15),(18,3),(13,18),(1,6),(7,13),(16,2),(20,6),(11,17),(9,16),(6,11),(10,15),(12,18),(19,6),(15,2),(10,17),(3,8),(5,11),(1,7),(13,19),(16,3),(7,14),(8,13),(17,2)], [1,6,1,6,4,1,5,4,4,2,2,3,3,5,4,3,1])
  ]

diceTests = zipWith (\n t -> TestLabel ("diceTest " ++ show n) t) [1..]
            (map diceTestCase diceTestData)

stDiceTestCase (expected, winningNode, edges, dieRolls) =
  TestCase (assertEqual (show expected ++ " " ++ show winningNode)
            expected (stDice winningNode edges dieRolls))

stDiceTests = zipWith (\n t -> TestLabel ("stDiceTest " ++ show n) t) [1..]
            (map stDiceTestCase diceTestData)

runTests = runTestTT (TestList (diceTests ++ stDiceTests))
