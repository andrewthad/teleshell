{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-imports #-}

module Pipes.ByteString.Substring
  ( consumeBreakSubstring
  ) where

import Pipes
import Data.Int
import Data.ByteString.Substring
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BB

consumeBreakSubstring :: Monad m => ByteString -> Consumer' ByteString m (Builder,ByteString)
consumeBreakSubstring pat = do
  (b0, leftovers) <- takeStrict (B.length pat)
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

-- | In the returned tuple, the first element is the bytestring prior to
--   the index. The second item is the leftover bytes in the chunk.
takeStrict :: Monad m => Int -> Consumer' ByteString m (ByteString,ByteString)
takeStrict total = if total < 1
  then return (B.empty,B.empty)
  else await >>= go 0 mempty
  where
  go :: Monad m => Int -> Builder -> ByteString -> Consumer' ByteString m (ByteString,ByteString)
  go i1 bb bs = do
    let i2 = i1 + B.length bs
    if i2 < total
      then await >>= go i2 (bb <> BB.byteString bs)
      else do
        let (a,b) = B.splitAt (B.length bs - (i2 - total)) bs
        return (LB.toStrict $ BB.toLazyByteString $ bb <> BB.byteString a, b)

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral

