import Data.List (transpose, isPrefixOf, tails)

horizontal :: String -> [String]
horizontal = lines

vertical :: String -> [String]
vertical input = transpose $ horizontal input

diagPos :: String -> [String]
diagPos input = diagonals $ horizontal input
  where
    diagonals matrix = [ [matrix !! (i + k) !! k | k <- [0 .. min (n - i - 1) (m - 1)]] | i <- [0 .. (n - 1)] ] ++
                       [ [matrix !! k !! (j + k) | k <- [0 .. min (n - 1) (m - j - 1)]] | j <- [1 .. (m - 1)] ]
      where
        n = length matrix
        m = length $ head matrix

reverseInput :: String -> String
reverseInput input = unlines $ map reverse $ horizontal input

diagNeg :: String -> [String]
diagNeg input = diagPos $ reverseInput input

substringOcurrences :: String -> String -> Int
substringOcurrences subs string = length $ filter (isPrefixOf subs) (tails string)

countXMAS :: [String] -> [Int]
countXMAS list = zipWith (+) (map (substringOcurrences "XMAS") list) (map (substringOcurrences "SAMX") list)

generateContainers :: String -> [[String]]
generateContainers input = [[take 3 (drop j (matrix !! (i + k))) | k <- [0..2]] | i <- [0..n-3], j <- [0..m-3]]
  where
    matrix = horizontal input
    n = length matrix
    m = length $ head matrix

validDiagonal :: [Char] -> Bool
validDiagonal [x, y] = x == 'M' && y == 'S' || x == 'S' && y == 'M'

validContainer :: [String] -> Bool
validContainer container = container !! 1 !! 1 == 'A' &&
                           validDiagonal [head (head container), container !! 2 !! 2] &&
                           validDiagonal [head (container !! 2), head container !! 2]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sum $ countXMAS $ concat [horizontal input, vertical input, diagPos input, diagNeg input]
    print $ length $ filter validContainer $ generateContainers input