import Data.List.Split
import Data.List (sort, intersect)

heads :: [a] -> [[a]]
heads [] = [[]]
heads xs = [take n xs | n <- [1..length xs]]

appliableRules :: [[Int]] -> Int -> [Int]
appliableRules rules number = sort $ map (\[x, y] -> y) $ filter (\[x, y] -> x == number) rules

validNumber :: [[Int]] -> [Int] -> Int -> Bool
validNumber rules list number = null (list `intersect` appliableRules rules number)

validManual :: [[Int]] -> [Int] -> Bool
validManual rules manual = all (\x -> validNumber rules x (x !! (length x - 1))) (heads manual)

middleElement :: [Int] -> Int
middleElement list = list !! (length list `div` 2)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rules = map (map (read :: String -> Int) . splitOn "|") $ lines $ head $ splitOn "\n\n" input
    let manuals = map (map (read :: String -> Int) . splitOn ",") $ lines $ splitOn "\n\n" input !! 1
    print $ sum $ map middleElement $ filter (validManual rules) manuals