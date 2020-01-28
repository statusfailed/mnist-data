module Data.MNIST where

import Data.Binary (Binary(..), decodeFile)
import Data.Binary.Get (getByteString)

import GHC.Word
import Control.Monad

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

-- | A file containing MNIST labels
data Labels = Labels
  { _labelsMagicNumber :: Word32
  , _labelsNumberItems :: Word32
  , _labelsLabels      :: [Word8]
  } deriving(Eq, Ord, Read, Show)

instance Binary Labels where
  put (Labels m n xs) = put m >> put n >> mapM_ put xs
  get = do
    m <- get
    when (m /= 0x801) $ fail "magic number /= 0x801"
    n <- get
    Labels m n <$> replicateM (fromIntegral n) get

-- | A file containing MNIST images
data Images = Images
  { _imagesMagicNumber :: Word32
  , _imagesNumberItems :: Word32
  , _imagesRows        :: Word32
  , _imagesCols        :: Word32
  , _imagesPixels      :: [BS.ByteString]
  } deriving(Eq, Ord, Read, Show)

instance Binary Images where
  put (Images m n r c is) = put m >> put n >> put r >> put c >> mapM_ put is
  get = do
    m <- get
    when (m /= 0x803) $ fail "magic number /= 0x801"
    (n, r, c) <- (,,) <$> get <*> get <*> get
    Images m n r c <$>
      replicateM (fromIntegral n) (getByteString $ fromIntegral (r * c))

readImagesFile :: FilePath -> IO Images
readImagesFile = decodeFile

readLabelsFile :: FilePath -> IO Labels
readLabelsFile = decodeFile

-- | Read images and labels from separate files.
readMnistDataset :: FilePath -> FilePath -> IO (Images, Labels)
readMnistDataset imgPath lblPath = do
  images <- readImagesFile imgPath
  labels <- readLabelsFile lblPath
  return (images, labels)
