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
import Network.Zhone
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Pipes.Prelude as PP

import Data.ByteString.Substring (breakSubstringLazy)
import Pipes.ByteString.Substring (consumeBreakSubstring,consumeDropExactLeftovers)
import Test.DocTest (doctest)

main :: IO ()
main = do
  doctest ["src/Network/Teleshell.hs"]
  defaultMain tests

traceShowId :: a -> a
traceShowId = id

tests :: TestTree
tests = testGroup "Tests"
  [ properties
  , substringUnitTests
  , parserUnitTests
  ]

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

substringUnitTests :: TestTree
substringUnitTests = testGroup "Substring Unit Tests"
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
  , testCase "L" $ checkDropExactLeftovers 1 "theman" "themanistheplan" @?= Right "istheplan"
  , testCase "M" $ checkDropExactLeftovers 3 "" "coolkidsgofirst" @?= Right "coolkidsgofirst"
  ]

parserUnitTests :: TestTree
parserUnitTests = testGroup "Parser Unit Tests"
  [ testCase "System Info" $ testDecode "sample/zhone_system_info.txt" decodeSystemInfo expectedSystemInfo
  , testCase "VLAN Table" $ testDecode "sample/zhone_vlan.txt" decodeVlans expectedVlans
  , testCase "Port Rate Limit" $ testDecode "sample/zhone_port_rate_limit.txt" decodeRateLimits expectedRateLimits
  ]

testDecode :: (Eq a, Show a) => String -> (LB.ByteString -> Maybe a) -> a -> IO ()
testDecode filename decode expected = do
  bs <- B.readFile filename
  let lbs = LB.fromStrict bs
  decode lbs @?= Just expected

expectedVlans :: Vector Vlan
expectedVlans = V.fromList
  [ Vlan 127 "CDE_Mgmt_Vlan" "Bridged" "Disable" (makeAvailability [])
  , Vlan 150 "Meter" "Bridged" "Disable" (makeAvailability [6])
  , Vlan 200 "Video" "Bridged" "Disable" (makeAvailability [2,4,5])
  , Vlan 300 "Internet" "Bridged" "Disable" (makeAvailability [])
  , Vlan 305 "internet305" "Bridged" "Disable" (makeAvailability [1])
  , Vlan 555 "Phone IBBS" "Bridged" "Disable" (makeAvailability [])
  ]

expectedRateLimits :: Vector RateLimit
expectedRateLimits = V.fromList
  [ RateLimit "eth0" "Fiber WAN" "Disable" 0 0 500
  , RateLimit "eth1" "LAN 1 - GigE" "Disable" 0 0 500
  , RateLimit "eth2" "LAN 2 - GigE" "Disable" 0 0 500
  , RateLimit "eth3" "LAN 3 - GigE" "Disable" 0 0 500
  , RateLimit "eth4" "LAN 4 - GigE" "Disable" 0 0 500
  , RateLimit "eth5" "LAN 5 - GigE" "Disable" 0 0 500
  , RateLimit "eth6" "LAN 6 - GigE" "Disable" 0 0 500
  ]

makeAvailability :: [Int] -> Vector (Maybe Taggedness)
makeAvailability xs = V.create $ do
  mv <- MV.replicate 7 Nothing
  MV.write mv 0 (Just Tagged)
  forM_ xs $ \ix -> do
    MV.write mv ix (Just Untagged)
  return mv

expectedSystemInfo :: SystemInfo
expectedSystemInfo = SystemInfo
  "ZNID-GE-4226-EL-CDE"
  11262880
  0
  "ZNTS00000000"
  ( BootloaderVersion
    ( VersionCons 1 $ VersionCons 0 $ VersionCons 38 $ VersionNil )
    ( VersionCons 114 $ VersionCons 185 $ VersionNil )
    ( VersionCons 3 $ VersionCons 1 $ VersionCons 294 $ VersionNil )
  )
  (VersionCons 3 $ VersionCons 1 $ VersionCons 294 $ VersionNil)
  (Timestamp 17 03 17 17 31)
  (VersionCons 3 $ VersionCons 1 $ VersionCons 282 $ VersionNil)


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

checkDropExactLeftovers :: Int -> ByteString -> ByteString -> Either (Either [Atom] (Int,ByteString)) ByteString
checkDropExactLeftovers chunkSize preface haystack =
  let chunks :: Producer ByteString Identity ()
      chunks = mapM_ (yield . B.pack) (chunksOf chunkSize (B.unpack haystack))
      (atoms,m) = runIdentity (PP.toListM' ((chunks >> return Nothing)  >-> pipeDropExact preface))
  in case m of
    Just pair -> Left (Right pair)
    Nothing -> case flattenAfters atoms of
      Just bss -> Right (B.concat bss)
      Nothing -> Left (Left atoms)

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

pipeDropExact :: Monad m => ByteString -> Pipe ByteString Atom m (Maybe (Int,ByteString))
pipeDropExact preface = do
  e <- consumeDropExactLeftovers B.empty preface 
  case e of
    Left pair -> return (Just pair)
    Right leftovers -> do
      yield (AtomAfter leftovers)
      let go = do
            a <- await
            yield (AtomAfter a)
      forever go

