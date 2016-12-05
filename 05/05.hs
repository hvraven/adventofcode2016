{-# LANGUAGE OverloadedStrings #-}

import Data.Binary
import Data.Digest.Pure.MD5
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 hiding (map,head,foldl)
import Data.Monoid
import Data.List (sort)
import Prelude hiding (take)
import qualified Data.Map.Strict as Map
import Numeric (readHex)

input = "ojvtpuvg"

password :: ByteString -> ByteString
password = take 8 . pack . password' 0
  where
    password' :: Int -> ByteString -> String
    password' i id = let (hash,j) = nextPassword i id
                     in index hash 5 : password' (j+1) id

password2 = password2' Map.empty 0
  where
    password2' :: Map.Map Int Char -> Int -> ByteString -> IO String
    password2' m i id = do
      let (hash, j) = nextPassword i id
      let pos = fst $ head $ readHex (unpack $ singleton (index hash 5))
      let upd = if pos < 8
                   then addNew m pos (index hash 6)
                   else m
      print $ unpack hash ++ " " ++ (printStatus upd)
      if Map.size upd == 8
         then return $ printStatus upd
         else password2' upd (j+1) id

addNew m pos val | pos `Map.member` m = m
                 | otherwise          = Map.insert pos val m

printStatus :: Map.Map Int Char -> String
printStatus = map snd . sort . Map.toList . flip (foldl ((\m -> \p -> addNew m p ' '))) [0..7]

nextPassword :: Int -> ByteString -> (ByteString, Int)
nextPassword i id | "00000" == take 5 (hash i id) = ((hash i id),i)
                  | otherwise                     = nextPassword (i+1) id
  where
    hash :: Int -> ByteString -> ByteString
    hash i id = pack $ show $ md5 $ id `append` pack (show i)

main = do
  --print $ password input
  final <- password2 input
  print final