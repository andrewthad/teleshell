{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Network.Zhone
  ( SystemInfo(..)
  , Version(..)
  , BootloaderVersion(..)
  , Timestamp(..)
  , Taggedness(..)
  , Vlan(..)
  , RateLimit(..)
  , decodeVlans
  , decodeRateLimits
  , decodeSystemInfo
  ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Text.Encoding (decodeUtf8')
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import Control.Applicative
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.ByteString.Lazy as ALB

data SystemInfo = SystemInfo
  { systemInfoModelNumber :: !Text
  , systemInfoSerialNumber :: {-# UNPACK #-} !Int
  , systemInfoRegistrationId :: {-# UNPACK #-} !Int
  , systemInfoFsanNumber :: !Text
  , systemInfoBootloaderVersion :: !BootloaderVersion
  , systemInfoFirmwareVersion :: !Version
  , systemInfoFirmwareVersionTimestamp :: !Timestamp
  , systemInfoAlternateFirmwareVersion :: !Version
  } deriving (Show,Eq)

data Version = VersionCons {-# UNPACK #-} !Int !Version | VersionNil
  deriving (Show,Eq)

data BootloaderVersion = BootloaderVersion !Version !Version !Version
  deriving (Show,Eq)

data Timestamp = Timestamp
  { timestampYear :: !Int
  , timestampMonth :: !Int
  , timestampDay :: !Int
  , timestampHour :: !Int
  , timestampMinute :: !Int
  } deriving (Show,Eq)

data Vlan = Vlan
  { vlanNumber :: !Int
  , vlanName :: !ByteString
  , vlanType :: !ByteString
  , vlanSecure :: !ByteString
  , vlanAvailability :: !(Vector (Maybe Taggedness))
    -- ^ This is a vector of length 7. The element at each index
    --   corresponding to the port with that number.
    --   For example: 0 is eth0 and 1 is eth1. The length
    --   invariant is not expressed by the type system. Sorry.
  } deriving (Show,Eq)

data RateLimit = RateLimit
  { rateLimitInterfaceName :: !ByteString
  , rateLimitAlias :: !ByteString
  , rateLimitAdminState :: !ByteString
  , rateLimitInboundRateMbps :: !Int
  , rateLimitOutboundRateMbps :: !Int
  , rateLimitBurstSizeKb :: !Int
  } deriving (Show,Eq)

-- | The word "taggedness" is almost certainly not a real word, but
--   it shows up in this patent: https://www.google.com/patents/US20080101380
data Taggedness = Tagged | Untagged
  deriving (Show,Eq)

-- newtype Interface = Interface { getInterface :: ByteString }
--   deriving (Eq,Ord,Show,Read,Hashable)


decodeSystemInfo :: LB.ByteString -> Maybe SystemInfo
decodeSystemInfo = ALB.maybeResult . ALB.parse (parserSystemInfo <* AB.endOfInput)

parserSystemInfo :: Parser SystemInfo
parserSystemInfo = pure SystemInfo
  <*  AB.skipSpace
  <*  AB.string "System Info"
  <*  datum "Model Number"
  <*> textUntilWhiteSpace
  <*  datum "Serial Number"
  <*> AB.decimal
  <*  datum "Registration ID"
  <*> AB.decimal
  <*  datum "FSAN Number"
  <*> textUntilWhiteSpace
  <*  datum "Bootloader Version"
  <*> parserBootloader
  <*  datum "Firmware Version"
  <*> parserFirmwareVersion
  <*  datum "Firmware Version Timestamp"
  <*> parserTimestamp
  <*  datum "Alternate Firmware Version"
  <*> parserFirmwareVersion
  <*  AB.skipSpace

datum :: ByteString -> Parser ()
datum name = AB.skipSpace >> AB.string name >> AB.skipSpace

parserTimestamp :: Parser Timestamp
parserTimestamp = Timestamp
  <$> (AB.take 2 >>= intToByteString)
  <*> (AB.take 2 >>= intToByteString)
  <*> (AB.take 2 >>= intToByteString)
  <*  AB.char '_'
  <*> (AB.take 2 >>= intToByteString)
  <*> (AB.take 2 >>= intToByteString)

intToByteString :: ByteString -> Parser Int
intToByteString bs = case BC.readInt bs of
  Nothing -> fail "not an int"
  Just (i,bs') -> if BC.null bs'
    then return i
    else fail "leftovers found while parsing int"

parserFirmwareVersion :: Parser Version
parserFirmwareVersion = AB.char 'S' *> parserVersion

parserVersion :: Parser Version
parserVersion = do
  n <- AB.decimal
  fmap (VersionCons n) go
  where
  go :: Parser Version
  go = do
    m <- AB.peekChar
    case m of
      Nothing -> return VersionNil
      Just c -> if c == '.'
        then do
          _ <- AB.anyChar
          n <- AB.decimal
          fmap (VersionCons n) go
        else return VersionNil

parserBootloader :: Parser BootloaderVersion
parserBootloader = BootloaderVersion
  <$> parserVersion
  <*  AB.char '-'
  <*> parserVersion
  <*  AB.char ' '
  <*  AB.char '('
  <*> parserVersion
  <*  AB.char ')'

textUntilWhiteSpace :: Parser Text
textUntilWhiteSpace = do
  bs <- AB.takeTill AB.isSpace
  case decodeUtf8' bs of
    Left _ -> fail "non-UTF8 text"
    Right t -> return t

decodeVlans :: LB.ByteString -> Maybe (Vector Vlan)
decodeVlans = ALB.maybeResult . ALB.parse (parserVlans <* AB.endOfInput)

parserVlans :: Parser (Vector Vlan)
parserVlans = do
  AB.skipSpace
  _ <- AB.string "VLAN Table"
  AB.skipSpace
  _ <- AB.string "VLAN  VLAN Name                Type          Secure  eth0    eth1    eth2    eth3    eth4    eth5    eth6"
  AB.skipSpace
  _ <- AB.string "----  ---------                ----          ------  ----    ----    ----    ----    ----    ----    ----"
  AB.skipSpace
  let go :: Parser [Vlan]
      go = do
        m <- AB.peekChar
        case m of
          Just c -> if (c >= '0' && c <= '9')
            then liftA2 (:) (parserVlan <* AB.skipSpace) go
            else return []
          Nothing -> return []
  fmap V.fromList go

decodeRateLimits :: LB.ByteString -> Maybe (Vector RateLimit)
decodeRateLimits = ALB.maybeResult . ALB.parse (parserRateLimits <* AB.endOfInput)

parserRateLimits :: Parser (Vector RateLimit)
parserRateLimits = do
  AB.skipSpace
  _ <- AB.string "Port Rate Limit"
  AB.skipSpace
  _ <- AB.string "IfName  Alias          Admin State    Inbound Rate (Mbps)   Outbound Rate (Mbps)  Burst Size (KB)"
  AB.skipSpace
  _ <- AB.string "------  -----          -----------    --------------------  -------------------   ---------------"
  AB.skipSpace
  let go :: Parser [RateLimit]
      go = do
        m <- AB.peekChar
        case m of
          Just c -> if (c >= 'a' && c <= 'z')
            then liftA2 (:) (parserRateLimit <* AB.skipSpace) go
            else return []
          Nothing -> return []
  fmap V.fromList go

parserVlan :: Parser Vlan
parserVlan = constaparserToAttoparsec constaparserVlan

constaparserVlan :: Constaparser Vlan
constaparserVlan = Vlan
  <$> cpInt "VLAN" 6
  <*> cpByteString 25
  <*> cpByteString 14
  <*> cpByteString 9
  <*> withMutableVector 7 cpstPortsTaggedness

parserRateLimit :: Parser RateLimit
parserRateLimit = constaparserToAttoparsec constaparserRateLimit

constaparserRateLimit :: Constaparser RateLimit
constaparserRateLimit = RateLimit
  <$> cpByteString 8
  <*> cpByteString 15
  <*> cpByteString 15
  <*> cpInt "Inbound Rate" 22
  <*> cpInt "Outbound Rate" 22
  <*> cpIntSuffix "Burst Size" "KB" 18

cpInt :: String -> Int -> Constaparser Int
cpInt note n = Constaparser n $ \bs -> case BC.readInt (dropTrailingSpace bs) of
  Nothing -> Left (note ++ " string was: " ++ BC.unpack bs)
  Just (i,remaining) -> if BC.null remaining
    then Right i
    else Left note

cpIntSuffix :: String -> ByteString -> Int -> Constaparser Int
cpIntSuffix note suffix n = Constaparser n $ \bs -> 
  case BC.readInt (dropTrailingSpace bs) of
    Nothing -> Left (note ++ " string was: " ++ BC.unpack bs)
    Just (i,remaining) -> if remaining == suffix
      then Right i
      else Left note

-- cpEnabledVlan :: Constaparser Availability
-- cpEnabledVlan = Availability
--   <$> cpTaggedness "eth0" 8
--   <*> cpTaggedness "eth1" 8
--   <*> cpTaggedness "eth2" 8
--   <*> cpTaggedness "eth3" 8
--   <*> cpTaggedness "eth4" 8
--   <*> cpTaggedness "eth5" 8
--   <*> cpTaggedness "eth6" 7

cpstPortsTaggedness :: ConstaparserST s (MVector s (Maybe Taggedness)) ()
cpstPortsTaggedness = 
     cpstPortTaggedness "eth0" 0 8
  *> cpstPortTaggedness "eth1" 1 8
  *> cpstPortTaggedness "eth2" 2 8
  *> cpstPortTaggedness "eth3" 3 8
  *> cpstPortTaggedness "eth4" 4 8
  *> cpstPortTaggedness "eth5" 5 8
  *> cpstPortTaggedness "eth6" 6 7

cpstPortTaggedness :: String -> Int -> Int -> ConstaparserST s (MVector s (Maybe Taggedness)) ()
cpstPortTaggedness note ix n = ConstaparserST n $ \mv bs -> case dropTrailingSpace bs of
  "T" -> MV.write mv ix (Just Tagged) >> return (Right ())
  "U" -> MV.write mv ix (Just Untagged) >> return (Right ())
  "" -> MV.write mv ix Nothing >> return (Right ())
  _ -> return (Left note)

-- cpTaggedness :: String -> Int -> Constaparser (Maybe Taggedness)
-- cpTaggedness note n = Constaparser n $ \bs -> case dropTrailingSpace bs of
--   "T" -> Right (Just Tagged)
--   "U" -> Right (Just Untagged)
--   "" -> Right Nothing
--   _ -> Left note

cpByteString :: Int -> Constaparser ByteString
cpByteString n = Constaparser n (Right . dropTrailingSpace)

dropTrailingSpace :: ByteString -> ByteString
dropTrailingSpace = fst . BC.spanEnd (== ' ')

-- | Requires that the input string match the expected
--   length exactly.
-- runConstaparser :: Constaparser a -> ByteString -> Maybe a
-- runConstaparser (Constaparser n f) bs = do
--   guard (BC.length bs == n)
--   f bs

constaparserToAttoparsec :: Constaparser a -> Parser a
constaparserToAttoparsec (Constaparser n f) = do
  bs <- AB.take n
  case f bs of
    Left err -> fail ("constaparser failed: " ++ err)
    Right a -> return a

data Constaparser a = Constaparser {-# UNPACK #-} !Int (ByteString -> Either String a)
  deriving (Functor)

instance Applicative Constaparser where
  pure a = Constaparser 0 (const (Right a))
  Constaparser n f1 <*> Constaparser m f2 = Constaparser (n + m) $ \bs -> do
    let (bs1,bs2) = BC.splitAt n bs
    func <- f1 bs1
    val <- f2 bs2 -- (B.take m bs2)
    Right (func val)

data ConstaparserST s e a = ConstaparserST
  {-# UNPACK #-} !Int 
  (e -> ByteString -> ST s (Either String a))
  deriving (Functor)

instance Applicative (ConstaparserST s e) where
  pure a = ConstaparserST 0 (\_ _ -> return (Right a))
  ConstaparserST n f1 <*> ConstaparserST m f2 = ConstaparserST (n + m) $ \e bs -> do
    let (bs1,bs2) = BC.splitAt n bs
    efunc <- f1 e bs1
    case efunc of
      Left err -> return (Left err)
      Right func -> do
        eval <- f2 e bs2 -- (B.take m bs2)
        case eval of
          Left err -> return (Left err)
          Right val -> return (Right (func val))

withMutableVector :: 
     Int -- ^ Size of the mutable vector
  -> (forall s. ConstaparserST s (MVector s a) ())
  -> Constaparser (Vector a)
withMutableVector sz cp@(ConstaparserST n _) = Constaparser n $ \bs -> runST $ do
  case cp of
    ConstaparserST _ f -> do
      mv <- MV.new sz
      e <- f mv bs
      case e of
        Left err -> return (Left err)
        Right () -> do
          v <- V.unsafeFreeze mv
          return (Right v)

