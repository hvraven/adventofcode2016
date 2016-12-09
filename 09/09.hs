G{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Prelude hiding (take)

decompress = liftM concat (many decompress')
  where
    decompress' = decompressBlock <|> some (notChar '(')

decompressBlock = do
  (n,m) <- blockStart
  dat <- count n anyChar
  return $ concat $ replicate m dat

blockStart = do
  char '('
  n <- decimal
  char 'x'
  m <- decimal
  char ')'
  return (n,m)

unpartial p = case p of
                Partial r -> r ""

recursiveDecompress c i = do
  let d = head $ rights [parseOnly decompress i]
  let l = length d
  print l
  if c == l
     then print "done"
     else recursiveDecompress l $ BS.pack d

main = do
  input <- BS.getContents
  print $ length <$> parseOnly decompress input