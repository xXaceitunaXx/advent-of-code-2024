import qualified Data.Set as Set

type Position = (Int, Int)
data Direction = U | D | L | R deriving (Eq, Show)

turn :: Direction -> Direction
turn U = R
turn R = D
turn D = L
turn L = U

move :: Position -> Direction -> Position
move (i, j) U = (i - 1, j)
move (i, j) D = (i + 1, j)
move (i, j) L = (i, j - 1)
move (i, j) R = (i, j + 1)

parseInput :: [String] -> (Set.Set Position, (Position, Direction), (Int, Int))
parseInput lines = (obstacles, (guardPosition, guardDirection), (length lines, length $ head lines))
    where obstacles = Set.fromList [(i, j) | (i, obstacleRow) <- zip [0 ..] lines, (j, element) <- zip [0 ..] obstacleRow, element == '#']
          guardPosition = head [(i, j) |(i, guardRow) <- zip [0 ..] lines, (j, element) <- zip [0 ..] guardRow, element == '^' || element == '<' || element == '>' || element == 'v']
          guardDirection = case lines !! fst guardPosition !! snd guardPosition of 
            '^' -> U 
            'v' -> D 
            '<' -> L
            '>' -> R

visitedNumber :: Set.Set Position -> (Position, Direction) -> (Int, Int) -> Set.Set Position -> Set.Set Position 
visitedNumber obstacles (position, direction) (heigth, width) visitedPositions
    | not (inBounds position) = visitedPositions
    | otherwise = visitedNumber obstacles (newPosition, newDirection) (heigth, width) (Set.insert position visitedPositions)
    where inBounds (i, j) = i >= 0 && j >= 0 && i < heigth && j < width
          newPosition = if Set.member (move position direction) obstacles then position else move position direction
          newDirection = if Set.member (move position direction) obstacles then turn direction else direction

main :: IO ()
main = do
    input <- readFile "test.txt"
    let (obstacles, guard, dimensions) = parseInput (lines input)
    print $ Set.size $ visitedNumber obstacles guard dimensions Set.empty