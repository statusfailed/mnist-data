module Main where

import Data.MNIST

import GHC.Word (Word8)
import Data.Bits ((.|.), shiftL)
import Control.Monad ((<=<))
import Text.Printf (printf)
import System.Environment (getArgs)

import qualified Data.ByteString as BS

-- | Turn a 'BS.ByteString' into a bitvector, stored as an Integer with its
-- least-significant-bit as the 0th bit of the vector.
bitVector :: BS.ByteString -> Integer
bitVector bs = snd $ BS.foldl f (0, 0) bs
  where
    f (n, acc) pixel = (n+1, acc .|. (fromIntegral pixel `shiftL` (n*8)))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

printExample :: BS.ByteString -> Word8 -> IO ()
printExample image label = do
  putStr "LABEL: "
  print label
  putStrLn "IMAGE: "
  -- NOTE: most-significant-bits are the "end" of the bitvector, and so appear
  -- *first* when printing as a hexadecimal number- because we write numbers
  -- backwards. That's why we @reverse@ before chunking.
  -- The (*2) is because each pixel byte is printed as TWO chars (0-f)
  mapM_ putStrLn . chunksOf (28*2) . reverse $ printf "%.1568x" (bitVector image)

-- | print 
main = do
  [imagesFile, labelsFile] <- getArgs
  (i, l) <- readMnistDataset imagesFile labelsFile
  mapM_ f $ zip (_imagesPixels i) (_labelsLabels l)
  where f = const (putStrLn "press enter..." >> getLine) <=< uncurry printExample
