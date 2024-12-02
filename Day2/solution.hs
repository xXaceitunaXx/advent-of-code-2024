parseNumbers :: String -> [Int]
parseNumbers line = map read $ words line

differences :: [Int] -> [Int]
differences list = zipWith (-) list (tail list)

removeOne :: [Int] -> [[Int]]
removeOne list = [take i list ++ drop (i + 1) list | i <- [0..(length list - 1)]]

isSafe :: [Int] -> Bool
isSafe reactor = safetyCondition diff || or (map safetyCondition diffRemoved)
    where diff = differences reactor
          diffRemoved = map differences $ removeOne reactor
          safetyCondition list = and [(length $ filter (\x -> x > 3 || x < 1) (map abs list)) == 0, ((abs $ sum list) == (sum $ map abs list))]

main :: IO()
main = do
    input <- readFile "input.txt" -- use lines input
    let numberInput =  map parseNumbers $ lines input
    print $ length $ filter isSafe numberInput
