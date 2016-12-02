import Numeric (showHex)

findButton :: Int -> [Char] -> Int
findButton = foldl move

move :: Int -> Char -> Int
move x 'U' | x <= 3         = x
           | otherwise      = x - 3
move x 'D' | x >= 7         = x
           | otherwise      = x + 3
move x 'L' | x `mod` 3 == 1 = x
           | otherwise      = x - 1
move x 'R' | x `mod` 3 == 0 = x
           | otherwise      = x + 1

getSequence = map (findButton 5)

moveWeird x 'U' | x `elem` [6,7,8,10,11,12] = x - 4
                | x `elem` [3,13]           = x - 2
                | otherwise                 = x
moveWeird x 'D' | x `elem` [2,3,4,6,7,8]    = x + 4
                | x `elem` [1,11]           = x + 2
                | otherwise                 = x
moveWeird x 'L' | x `elem` [1,2,5,10,13]    = x
                | otherwise                 = x - 1
moveWeird x 'R' | x `elem` [1,4,9,12,13]    = x
                | otherwise                 = x + 1

getWeirdSequence = map (foldl moveWeird 5)

main = do
  input <- getContents
  print $ concat $ map show $ getSequence (lines input)
  print $ concat $ map (flip showHex "") $ getWeirdSequence (lines input)