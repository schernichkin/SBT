{-# LANGUAGE RecordWildCards #-}

module CSVConverter.FancyCSV
  ( ToCSVArgs (..)
  , toCSV
  ) where

import           Control.Monad.State.Strict
import           Data.Binary.Get
import           Pipes
import           Pipes.Binary
import           Pipes.ByteString
import           Pipes.Parse                (isEndOfInput, parsed)
import           System.IO
import           System.TimeIt

data ToCSVArgs = ToCSVArgs
  { _toCSVInput  :: !FilePath
  , _toCSVOutput :: !FilePath
  } deriving ( Show )

{-# INLINE fancyGet #-}
fancyGet :: Get ByteString
fancyGet = do
  len <- getWord16be
  skip 3
  getByteString (fromIntegral len - 2)

{-# INLINE fancyProducer #-}
fancyProducer :: Handle -> Producer ByteString IO ()
fancyProducer h = do
  (DecodingError consumed msg, remains) <- parsed (decodeGet fancyGet) (fromHandle h)
  when (consumed /= 0) $ fail msg
  eof <- lift $ evalStateT isEndOfInput remains
  unless eof $ fail msg

toCSV :: ToCSVArgs -> IO ()
toCSV ToCSVArgs {..} = do
  (elapsed, _) <- timeItT $
    withBinaryFile _toCSVInput  ReadMode  $ \input ->
    withBinaryFile _toCSVOutput WriteMode $ \output ->
      runEffect $ fancyProducer input >-> toHandle output
  putStrLn $ "total time:" ++ show elapsed
