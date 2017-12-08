import Control.Monad (forM)
import Data.Array ((!))
import Data.Graph (Graph, buildG)
import Data.List (minimumBy, maximumBy, nub)
import Data.Ord (comparing)

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
       else foo
  where Just (DieRoll d) = getCurrentDie gameState
        gameState' = updateGameStateWithPositionSeen gameState (getCurrentDieNumber gameState) startNodes
        gameState'' = updateGameStateToNextDie gameState'
        endNodes = traverseToDepth graph startNodes d
        foo = dice' graph winningNode endNodes (moveNumber + 1) gameState''

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
