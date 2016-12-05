{-# LANGUAGE OverloadedStrings #-}

import Data.Binary
import Data.Digest.Pure.MD5
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8
import Data.Monoid
import Prelude hiding (take)


input = "ojvtpuvg"

password :: ByteString -> ByteString
password = take 8 . pack . password' 0
  where
    password' :: Int -> ByteString -> String
    password' i id | "00000" == take 5 (hash i id) = index (hash i id) 5 : password' (i+1) id
                   | otherwise = password' (i+1) id
    hash :: Int -> ByteString -> ByteString
    hash i id = pack $ show $ md5 $ id `append` pack (show i)

main = do
  print $ password input