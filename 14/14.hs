{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow (first, (&&&))
import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Text.Regex.Posix

input = "cuanljph"

makeHash :: Int -> Digest MD5
makeHash = hashlazy . toLazyByteString . mappend input . intDec

makeStretchedHash :: Int -> Digest MD5
makeStretchedHash a = iterate (hash . B.pack . show) (makeHash a) !! 2016

stream :: [String]
stream = [show $ makeStretchedHash a | a <- [0..]]

isKey :: [String] -> Bool
isKey = fromMaybe False . fmap (uncurry quintupleCheck) . trippleCheck

trippleCheck :: [String] -> Maybe (Char,[String])
trippleCheck a = (\m -> (head m,tail a)) <$> (matchM regex) (head a)
  where
    regex = makeRegex ("(.)\\1\\1" :: String) :: Regex

quintupleCheck :: Char -> [String] -> Bool
quintupleCheck c xs = any (isInfixOf (replicate 5 c)) (take 1000 xs)

keys = checkKeys stream [0..]
  where
    checkKeys keys pos
      | isKey keys = (head keys,head pos) : checkKeys (tail keys) (tail pos)
      | otherwise  =                        checkKeys (tail keys) (tail pos)

main = do
  print $ Prelude.take 64 keys
