{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Word
import Data.List
import Data.Ord
import Data.Monoid
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Control.Monad
import Pipes
import Data.List.Split (chunksOf)

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Pipes.Prelude as PP

import Data.ByteString.Substring (breakSubstringLazy)
import Pipes.ByteString.Substring (consumeBreakSubstring)

main :: IO ()
main = defaultMain tests

traceShowId :: a -> a
traceShowId = id

tests :: TestTree
tests = testGroup "Tests" [properties,unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "breakSubstringLazy behaves like breakSubstring (big chunks)" $ do
      len1 <- choose (0,200) 
      len2 <- choose (0,200) 
      len3 <- choose (0,19) 
      lb1 <- fmap LB.pack (vector (traceShowId len1) :: Gen [Word8])
      lb2 <- fmap LB.pack (vector (traceShowId len2) :: Gen [Word8])
      lpat <- fmap LB.pack (vector (traceShowId len3) :: Gen [Word8])
      let pat = LB.toStrict lpat
          !lb = traceShowId (traceShowId lb1 <> traceShowId lpat <> traceShowId lb2)
      return (traceShowId (bimap LB.toStrict LB.toStrict (breakSubstringLazy pat lb)) == traceShowId (B.breakSubstring pat (LB.toStrict lb)))
  , QC.testProperty "breakSubstringLazy behaves like breakSubstring (small chunks)" $ do
      len1 <- choose (0,200) 
      len2 <- choose (0,200) 
      len3 <- choose (0,19) 
      lb1 <- fmap (LB.fromChunks . map B.singleton) (vector len1 :: Gen [Word8])
      lb2 <- fmap (LB.fromChunks . map B.singleton) (vector len2 :: Gen [Word8])
      lpat <- fmap (LB.fromChunks . map B.singleton) (vector len3 :: Gen [Word8])
      let pat = LB.toStrict lpat
          lb = lb1 <> lpat <> lb2
      return ((bimap LB.toStrict LB.toStrict (breakSubstringLazy pat lb)) == B.breakSubstring pat (LB.toStrict lb))
  , QC.testProperty "consumeBreakSubstring behaves like breakSubstring" $ do
      len1 <- choose (0,20) 
      len2 <- choose (0,20) 
      len3 <- choose (0,5) 
      chunkSize <- choose (1,6) 
      b1 <- fmap (B.concat . map B.singleton) (vector len1 :: Gen [Word8])
      b2 <- fmap (B.concat . map B.singleton) (vector len2 :: Gen [Word8])
      pat <- fmap (B.concat . map B.singleton) (vector len3 :: Gen [Word8])
      let b = b1 <> pat <> b2
      return (checkConsumer chunkSize pat b == Right (B.breakSubstring pat b))
  ]

unitTests = testGroup "Unit tests"
  [ testCase "A" $ breakSubstringLazy "" "thedrewlives" @?= ("","thedrewlives")
  , testCase "B" $ breakSubstringLazy "drew" "thedrewlives" @?= ("the","drewlives")
  , testCase "C" $ breakSubstringLazy "drew" "thelivesofcatsarewhatdrewstudiesonfriday" @?= ("thelivesofcatsarewhat","drewstudiesonfriday")
  , testCase "D" $ breakSubstringLazy "jordan" "thelivesofcatsarewhatjordanstudiesonfriday" @?= ("thelivesofcatsarewhat","jordanstudiesonfriday")
  , testCase "E" $ breakSubstringLazy "a" "ababab" @?= ("","ababab")
  , testCase "F" $ breakSubstringLazy "aaaaaaa" "aaaa" @?= ("aaaa","")
  , testCase "G" $ checkConsumer 1 "drew" "thetopdrewperson" @?= Right ("thetop","drewperson")
  , testCase "H" $ checkConsumer 1 "ronnie" "thepersonnamedronnieisthebest" @?= Right ("thepersonnamed","ronnieisthebest")
  , testCase "I" $ checkConsumer 3 "drew" "thetopdrewperson" @?= Right ("thetop","drewperson")
  , testCase "J" $ checkConsumer 4 "ronnie" "thepersonnamedronnieisthebest" @?= Right ("thepersonnamed","ronnieisthebest")
  , testCase "K" $ checkConsumer 5 "" "the" @?= Right ("","the")
  ]

checkConsumer :: Int -> ByteString -> ByteString -> Either [Atom] (ByteString,ByteString)
checkConsumer chunkSize needle haystack =
  let chunks :: Producer ByteString Identity ()
      chunks = mapM_ (yield . B.pack) (chunksOf chunkSize (B.unpack haystack))
      results = PP.toList (chunks >-> pipeBreakSubstring needle)
  in case results of
    AtomBefore bs : afters -> case flattenAfters afters of
      Just bss -> Right (bs, B.concat bss)
      Nothing -> Left results
    _ -> Left results

data Atom = AtomBefore ByteString | AtomAfter ByteString
  deriving (Show,Eq)

flattenAfters :: [Atom] -> Maybe [ByteString]
flattenAfters [] = Just []
flattenAfters (AtomBefore _ : _) = Nothing
flattenAfters (AtomAfter bs : xs) = do
  ys <- flattenAfters xs
  Just (bs : ys)

pipeBreakSubstring :: Monad m => ByteString -> Pipe ByteString Atom m ()
pipeBreakSubstring pat = do
  (pre,post) <- consumeBreakSubstring pat
  yield (AtomBefore $ LB.toStrict $ BB.toLazyByteString pre)
  yield (AtomAfter post)
  let go = do
        a <- await
        yield (AtomAfter a)
  forever go


