{-# LANGUAGE Rank2Types      #-}
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
import           System.IO
import           System.TimeIt

data ToCSVArgs = ToCSVArgs
  { _toCSVInput  :: FilePath
  , _toCSVOutput :: FilePath
  } deriving ( Show )

{-# INLINE fancyGet #-}
fancyGet :: Get ByteString
fancyGet = do
  len <- getWord16be
  skip 3
  getByteString (fromIntegral len - 2)

{-# INLINE fancyProducer #-}
fancyProducer :: (MonadIO m) => Handle -> Producer' ByteString m ()
fancyProducer = go . fromHandle
  where
    go producer = do
      (result, remains) <- runStateT (decodeGet fancyGet) producer
      case result of
        Right row -> do
          yield row
          go remains
        Left err -> liftIO $ putStrLn $ deMessage err

toCSV :: ToCSVArgs -> IO ()
toCSV ToCSVArgs {..} = do
  (elapsed, _) <- timeItT $
    withBinaryFile _toCSVInput  ReadMode  $ \input ->
    withBinaryFile _toCSVOutput WriteMode $ \output ->
      runEffect $ fancyProducer input >-> toHandle output
  putStrLn $ "total time:" ++ show elapsed
