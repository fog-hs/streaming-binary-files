{-# Language AllowAmbiguousTypes,BangPatterns,RankNTypes,TypeApplications,ScopedTypeVariables #-}

import Data.ByteString.Lazy (hPut,hGetContents,ByteString)
import System.IO (openBinaryFile,IOMode(..),hClose)
import Data.Binary
import Stream.ListT
import Stream.LinearM

writeBinaryStream :: Binary a => FilePath -> [a] -> IO ()
writeBinaryStream path xs = do
 h <- openBinaryFile path AppendMode
 writeBinaryStream' h xs  
 hClose h
  where
   writeBinaryStream' h [] = return ()
   writeBinaryStream' h (x:xs) = do
    !_ <- hPut h (encode x) 
    writeBinaryStream' h xs

writeBinaryStreamM :: Binary a => FilePath -> IOList a -> IO ()
writeBinaryStreamM path xs = do
 h <- openBinaryFile path AppendMode
 foldrM (\a -> (const <$> return (hPut h (encode a)))) () xs
 hClose h
  where

unConsBinaryStream :: forall a m. (Binary a,Monad m) => ByteString
           -> m (Maybe (a, ByteString))
unConsBinaryStream xs = case 
    decodeOrFail @a xs
     of
      Left _ -> return Nothing
      Right (xs,_,a) -> return (Just (a,xs))

readBinaryStream :: forall a. Binary a => FilePath -> IOList a
readBinaryStream path = unfoldrM unConsBinaryStream (openBinaryFile path ReadMode >>= hGetContents)
                    
copyBinaryStream :: forall a. Binary a => FilePath -> FilePath -> IO ()
copyBinaryStream path path2 = writeBinaryStreamM path2 $ readBinaryStream @a path

testWrite = writeBinaryStream "test.bin" [1:: Int .. 10000000]
testRead = display $ readBinaryStream @Int "test.bin" 
testCopy = copyBinaryStream @Int "test.bin" "copy.bin" 
