{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-imports #-}

module Pipes.ByteString.Substring
  ( consumeBreakSubstring
  , consumeBreakSubstringLeftovers
  , consumeDropExactLeftovers
  , consumeDropWhileLeftovers
  ) where

import Pipes
import Data.Int
import Data.ByteString.Substring
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC

consumeBreakSubstring :: Monad m => ByteString -> Consumer' ByteString m (Builder,ByteString)
consumeBreakSubstring = consumeBreakSubstringLeftovers B.empty

consumeBreakSubstringLeftovers :: Monad m => ByteString -> ByteString -> Consumer' ByteString m (Builder,ByteString)
consumeBreakSubstringLeftovers leftovers0 pat = do
  (b0, leftovers) <- takeStrictLeftovers leftovers0 (B.length pat)
  let !s = prepareBreakSubstring pat b0
  go s mempty (LB.fromStrict b0) leftovers
  where
  go :: Monad m
     => KarpRabinState 
     -> Builder 
     -> LB.ByteString -- Buffer for chunks we cannot yet append to the builder
     -> ByteString
     -> Consumer' ByteString m (Builder,ByteString)
  go s1 bb heldChunk chunk = case breakSubstringResume s1 chunk of
    KarpRabinResultDone ix _ -> return $ if ix < 0
      then let (a,b) = LB.splitAt (LB.length heldChunk + intToInt64 ix) heldChunk in
        (bb <> BB.lazyByteString a, LB.toStrict b <> chunk)
      else let (a,b) = B.splitAt ix chunk in
        (bb <> BB.lazyByteString heldChunk <> BB.byteString a, b)
    KarpRabinResultMore s2 -> do
      let appendedHeldChunk = heldChunk <> LB.fromStrict chunk
          (confirmedChunk,nextHeldChunk) = LB.splitAt (LB.length appendedHeldChunk - intToInt64 (B.length pat)) appendedHeldChunk
      nextChunk <- await
      go s2 (bb <> BB.lazyByteString confirmedChunk) nextHeldChunk nextChunk

-- | If we get back a Left, then the chunks did not match what we expected.
--   The tuple contains the number of characters that did match and the
--   beginning of the failure to match.
--   If we get back a Right, it has the leftovers from the chunk that
--   completed the match.
consumeDropExactLeftovers :: Monad m => ByteString -> ByteString -> Consumer' ByteString m (Either (Int,ByteString) ByteString)
consumeDropExactLeftovers leftovers0 preface = go 0 leftovers0
  where
  go :: Monad m => Int -> ByteString -> Consumer' ByteString m (Either (Int,ByteString) ByteString)
  go ix chunk = if lenRemainingPreface > lenChunk
    then do
      let (p1,_) = B.splitAt lenChunk remainingPreface
      if p1 == chunk
        then await >>= go (ix + lenChunk)
        else do
          let ixDifferentByte = findDifferentByte p1 chunk
          return (Left (ixDifferentByte + ix,B.drop ixDifferentByte chunk))
    else do
      let (c1,c2) = B.splitAt lenRemainingPreface chunk
      if c1 == remainingPreface
        then return (Right c2)
        else do
          let ixDifferentByte = findDifferentByte c1 remainingPreface
          return (Left (ixDifferentByte + ix,B.drop ixDifferentByte chunk))
    where
    remainingPreface = B.drop ix preface
    lenRemainingPreface = B.length remainingPreface
    lenChunk = B.length chunk

consumeDropWhileLeftovers :: Monad m => ByteString -> (Char -> Bool) -> Consumer' ByteString m ByteString
consumeDropWhileLeftovers leftovers0 predicate = go leftovers0
  where
  go :: Monad m => ByteString -> Consumer' ByteString m ByteString
  go chunk = do
    let remaining = BC.dropWhile predicate chunk
    if B.null remaining
      then await >>= go
      else return remaining

-- | This is extremely inefficient. Returns -10000 if all bytes match.
findDifferentByte :: ByteString -> ByteString -> Int
findDifferentByte a b = fromMaybe (-10000) (L.elemIndex False (B.zipWith (==) a b))

-- | In the returned tuple, the first element is the bytestring prior to
--   the index. The second item is the leftover bytes in the chunk.
takeStrictLeftovers :: Monad m => ByteString -> Int -> Consumer' ByteString m (ByteString,ByteString)
takeStrictLeftovers leftovers0 total = if total < 1
  then return (B.empty,leftovers0)
  else go 0 mempty leftovers0
  where
  go :: Monad m => Int -> Builder -> ByteString -> Consumer' ByteString m (ByteString,ByteString)
  go i1 bb bs = do
    let i2 = i1 + B.length bs
    if i2 < total
      then await >>= go i2 (bb <> BB.byteString bs)
      else do
        let (a,b) = B.splitAt (B.length bs - (i2 - total)) bs
        return (LB.toStrict $ BB.toLazyByteString $ bb <> BB.byteString a, b)

-- takeStrict :: Monad m => Int -> Consumer' ByteString m (ByteString,ByteString)
-- takeStrict = takeStrictLeftovers B.empty

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral

