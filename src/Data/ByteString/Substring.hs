{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-imports #-}

module Data.ByteString.Substring
  ( breakSubstringLazy
  , prepareBreakSubstring
  , breakSubstringResume
  , KarpRabinState
  , KarpRabinResult(..)
  ) where

import Prelude hiding (length,null)
import Data.Bits
import Data.Word
import Data.Int
import Data.ByteString
import Data.ByteString.Unsafe
import Data.Primitive.ByteArray
import Control.Monad.ST (RealWorld)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LBI

import Data.Char (chr)

breakSubstringLazy :: 
     ByteString -- ^ needle, strict bytestring
  -> LB.ByteString -- ^ haystack, lazy bytestring
  -> (LB.ByteString,LB.ByteString)
breakSubstringLazy pat lb =
  let !lp = intToInt64 (length pat)
      !b0 = LB.toStrict (LB.take lp lb)
      !lb' = LB.drop lp lb
      !s = prepareBreakSubstring pat b0
   in case go s lb' of
        BreakBackwardDone pre post -> (if null b0 then pre else LBI.Chunk b0 pre,post)
        BreakBackwardBy !n -> let (bpre,bpost) = unsafeSplitAt (length b0 - n) b0 in
          (LB.fromStrict bpre,if null bpost then lb' else LBI.Chunk bpost lb')
  where
  go :: KarpRabinState -> LB.ByteString -> BreakBackward -- (LB.ByteString,LB.ByteString)
  go _ LBI.Empty = BreakBackwardDone LBI.Empty LBI.Empty
  go s1 (LBI.Chunk !c cs) = case breakSubstringResume s1 c of
    KarpRabinResultMore s2 -> case go s2 cs of
      BreakBackwardBy n -> if n <= length c
        then let (ca,cb) = unsafeSplitAt (length c - n) c in
          BreakBackwardDone
            (if null ca then LBI.Empty else LBI.Chunk ca LBI.Empty)
            (if null cb then LBI.Empty else LBI.Chunk cb cs)
        else BreakBackwardBy (n - length c)
      BreakBackwardDone cs' cs'' -> BreakBackwardDone (LBI.Chunk c cs') cs''
    KarpRabinResultDone ix _ -> if ix < 0
      then BreakBackwardBy (negate ix)
      else let (ca,cb) = unsafeSplitAt ix c in
        BreakBackwardDone
          (if null ca then LBI.Empty else LBI.Chunk ca LBI.Empty)
          (LBI.Chunk cb cs)

prepareBreakSubstring :: 
     ByteString -- ^ needle
  -> ByteString -- ^ first n characters in haystack, where n is length of needle
  -> KarpRabinState
prepareBreakSubstring pat b0 =
  let !lp = intToInt64 (length pat)
      !log2BufSize = finiteBitSize (0 :: Int) - countLeadingZeros (length pat)
      !bufSize = 2 ^ log2BufSize
      !buf = unsafePerformIO (newBuffer bufSize b0)
      !m = 2891336453 ^ length pat
      !hp = rollingHash pat
      !hs0 = rollingHash b0
   in (KarpRabinState hp m pat hs0 log2BufSize (bufSize - 1) buf lp)

rollingHash :: ByteString -> Word32
rollingHash = foldl' (\h b -> h * 2891336453 + word8ToWord32 b) 0

data BreakBackward
  = BreakBackwardDone LB.ByteString LB.ByteString
  | BreakBackwardBy !Int
    -- ^ The int in here should always be positive

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral

int64ToInt :: Int64 -> Int
int64ToInt = fromIntegral

word8ToWord32 :: Word8 -> Word32
word8ToWord32 = fromIntegral

breakSubstringResume :: KarpRabinState -> ByteString -> KarpRabinResult
breakSubstringResume (KarpRabinState hp m pat hs0 log2BufSize bufMask buf i64Start) chunk =
  unsafePerformIO (search hs0 i64Start)
  where
    k = 2891336453 :: Word32
    readByteArrayMod :: Int -> IO Word8
    readByteArrayMod ix = readByteArray buf (ix .&. bufMask)
    writeByteArrayMod :: Int -> Word8 -> IO ()
    writeByteArrayMod ix = writeByteArray buf (ix .&. bufMask)
    search :: Word32 -> Int64 -> IO KarpRabinResult
    search !hs !i64 = do
      let !i = int64ToInt (i64 - i64Start)
          keepGoing :: IO KarpRabinResult
          keepGoing = do
            oldVal <- readByteArrayMod (int64ToInt (i64 - intToInt64 (length pat)))
            let w8 :: Word8
                !w8 = unsafeIndex chunk i
                !hs' = hs * k + fromIntegral w8 - m * fromIntegral oldVal
            writeByteArrayMod (int64ToInt i64) w8
            search hs' (i64 + 1)
      if hp == hs
        then do
          b <- mutableByteArrayEqByteString
            (bufMask .&. (int64ToInt i64 - length pat))
            bufMask buf pat
          if b
            then return (KarpRabinResultDone (i - length pat) (i64 - fromIntegral (length pat)))
            else keepGoing 
        else if length chunk <= i 
          then return (KarpRabinResultMore (KarpRabinState hp m pat hs log2BufSize bufMask buf i64))
          else keepGoing

unsafeSplitAt :: Int -> ByteString -> (ByteString,ByteString)
unsafeSplitAt i s = (unsafeTake i s, unsafeDrop i s)

-- | You must provide a size equal to or larger than the
--   ByteString length
newBuffer :: Int -> ByteString -> IO (MutableByteArray RealWorld)
newBuffer sz bs = do
  arr <- newByteArray sz
  copyIntoBuffer bs arr
  return arr

copyIntoBuffer :: ByteString -> MutableByteArray RealWorld -> IO ()
copyIntoBuffer bs arr = go 0
  where
  go :: Int -> IO ()
  go ix = if ix < length bs
    then do
      let !w = unsafeIndex bs ix
      writeByteArray arr ix w
      go (ix + 1)
    else return ()

mutableByteArrayEqByteString :: Int -> Int -> MutableByteArray RealWorld -> ByteString -> IO Bool
mutableByteArrayEqByteString arrIx bufMask arr bs = go 0
  where
  readByteArrayMod :: Int -> IO Word8
  readByteArrayMod ix = readByteArray arr (ix .&. bufMask)
  go :: Int -> IO Bool
  go i = if i < length bs
    then do
      w1 <- readByteArrayMod (arrIx + i)
      let w2 = unsafeIndex bs i
      if w1 == w2 then go (i + 1) else return False
    else return True

data KarpRabinState = KarpRabinState
  !Word32 -- pattern fingerprint
  !Word32 -- constant k exponentiated
  !ByteString -- pattern
  !Word32 -- current fingerprint
  !Int -- log base 2 of buffer size
  !Int -- mask
  !(MutableByteArray RealWorld) -- current buffer, contains end of previous bytestring
  !Int64 -- total number of bytes consumed, also works as buffer index 
         -- after doing some modular arithmetic

data KarpRabinResult
  = KarpRabinResultDone !Int !Int64
    -- ^ The first number is the index into the current chunk.
    --   The second number is the total number of characters
    --   that were consumed. Note that since these both refer
    --   to the index of the beginning of the match, the first
    --   one is allowed to be negative, but the second is not.
    --   The third item is the bytes preceeding the match location.
    --   This is provided to help streaming providers that may have
    --   already discarded the old data.
  | KarpRabinResultMore !KarpRabinState

