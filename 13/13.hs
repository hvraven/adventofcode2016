{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

import Control.Arrow (second)
import Control.Lens
import Control.Lens.At
import Data.Bits (popCount)
import Data.List (mapAccumL)
import Data.Map.Strict (Map,(!))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes,fromJust)

data PathMap = Map (Int,Int) Int

input = 1350

isOpen :: (Int,Int) -> Bool
isOpen (x,y) | x < 0     = False
             | y < 0     = False
             | otherwise = even $ popCount $ (+ input) $ maths x y
  where
    maths x y = x*x+3*x+2*x*y+y+y*y

findPath start end = ret $ dropWhile (not . Map.member end . fst) 
                                     (steps (makeStart start) )
  where
    ret = (! end) . fst . head

makeStart start = ((Map.fromList [(start,0)]),[start])

steps = iterate (uncurry step)

step paths active = concat `second` mapAccumL updateNeighbours paths active

updateNeighbours paths pos = 
  catMaybes `second` mapAccumL (update distance) paths
                               (filter isOpen $ neighbours pos)
  where
    distance = 1 + (fromJust $ paths ^. at pos)

update n ps x = if ps ^. at x == Nothing
                  then (ps & at x ?~ n, Just x)
                  else (ps, Nothing)

neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

main = do
  print $ findPath (1,1) (31,39)
  print $ length $ fst $ (steps (makeStart (1,1)) !! 50)
