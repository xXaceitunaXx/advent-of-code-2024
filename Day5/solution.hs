import Data.List.Split
import Data.List (sort, intersect, delete, elemIndex)
import Data.Maybe (fromJust, isJust)
import Data.Foldable (find)

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

findGreatest :: [[Int]] -> [Int] -> Int
findGreatest rules manual = head $ filter (\x -> all (notElem x . appliableRules rules) manual) manual

reorder :: [[Int]] -> [Int] -> [Int]
reorder rules [] = []
reorder rules manual = greatest : reorder rules rest
  where
    greatest = findGreatest rules manual
    rest = delete greatest manual

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rules = map (map (read :: String -> Int) . splitOn "|") $ lines $ head $ splitOn "\n\n" input
    let manuals = map (map (read :: String -> Int) . splitOn ",") $ lines $ splitOn "\n\n" input !! 1

    print $ sum $ map middleElement $ filter (validManual rules) manuals
    print $ sum $ map (middleElement . reorder rules) (filter (not . validManual rules) manuals)
