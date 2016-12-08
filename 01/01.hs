import Text.Parsec
import Prelude hiding (Left,Right)
import Control.Monad (liftM)
import Data.Either (rights)
import Data.Set (empty,member,insert,elemAt)
import Control.Arrow

data Command = Left Integer
             | Right Integer
             deriving (Show)

data Direction = N
               | O
               | S
               | W
               deriving (Show,Eq)

navigation = sepBy order (string ", ")

order = (char 'L' *> liftM Left number) <|>
        (char 'R' *> liftM Right number)

number = liftM read (many digit)

walk :: (Integer,Integer,Integer) -> Command -> (Integer,Integer,Integer)
walk a b = uncurry move $ turn a b

turn :: (Integer,Integer,Integer) -> Command -> ((Integer,Integer,Integer),Integer)
turn (x,y,3) (Right z) = ((x,y,0), z)
turn (x,y,a) (Right z) = ((x,y,a+1), z)
turn (x,y,0) (Left z)  = ((x,y,3), z)
turn (x,y,a) (Left z)  = ((x,y,a-1), z)

move :: (Integer,Integer,Integer) -> Integer -> (Integer,Integer,Integer)
move (x,y,0) z = (x+z, y, 0)
move (x,y,1) z = (x, y+z, 1)
move (x,y,2) z = (x-z, y, 2)
move (x,y,3) z = (x, y-z, 3)

findDuplicate = findDuplicate' empty . expand . map dropDirection
  where
    findDuplicate' s (x:xs)
       | x `member` s = x
       | otherwise    = findDuplicate' (insert x s) xs
    findDuplicate' s [] = elemAt 0 s

expand = concat . map (tail . uncurry expand') . uncurry zip . (init &&& tail)
  where
    expand' (x1,y1) (x2,y2)
      | x1 == x2 = [(x1,y) | y <- range y1 y2]
      | y1 == y2 = [(x,y1) | x <- range x1 x2]

range a b | a <= b    = [a..b]
          | otherwise = enumFromThenTo a (a-1) b

dropDirection (a,b,c) = (a,b)

main = do
  input <- getContents
  let parsed = parse navigation "Error" input
  let trip = fmap (scanl walk (0,0,0)) parsed
  print $ trip
  print $ fmap last trip
  print $ fmap (expand . map dropDirection) trip
  print $ fmap findDuplicate trip