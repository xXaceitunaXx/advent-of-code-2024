import Data.List (transpose, isPrefixOf, tails)

horizontal :: String -> [String]
horizontal = lines

vertical :: String -> [String]
vertical input = transpose $ horizontal input

diagPos :: String -> [String]
diagPos input = filter (\x -> length x > 3) $ diagonals $ horizontal input
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

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sum $ countXMAS $ concat [horizontal input, vertical input, diagPos input, diagNeg input]