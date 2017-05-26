{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Teleshell
  ( runSocketPipe
  , runSocketPipeMaybe
  , runSocketPipeEither
  , teleshell
  , Exchange(..)
  , Command(..)
  ) where

import Prelude hiding (Proxy)
import Pipes.Core
import Pipes
import Network
import Data.ByteString (ByteString)
import Pipes.ByteString.Substring (consumeBreakSubstringLeftovers,consumeDropExactLeftovers,consumeDropWhileLeftovers)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.String
import Data.Monoid
import Data.Maybe
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket.ByteString as NSB

-- | The socket must already be connected.
runSocketPipe :: Socket -> Pipe ByteString ByteString IO () -> IO ()
runSocketPipe sock p = do
  runEffect (socketToProducer sock 4096 >-> p >-> socketToConsumer sock)

-- | The socket must already be connected.
runSocketPipeMaybe :: Socket -> Pipe ByteString ByteString (MaybeT IO) a -> IO (Maybe a)
runSocketPipeMaybe sock p = do
  runMaybeT (runEffect (socketToProducerMaybe sock 4096 >-> p >-> socketToConsumerMaybe sock))

-- | The socket must already be connected.
runSocketPipeEither :: 
     Socket
  -> Pipe ByteString ByteString (ExceptT TeleshellError IO) a
  -> IO (Either TeleshellError a)
runSocketPipeEither sock p = do
  runExceptT (runEffect (socketToProducerEither sock 4096 >-> p >-> socketToConsumerEither sock))

socketToProducer :: Socket -> Int -> Producer ByteString IO ()
socketToProducer sock nbytes = loop
  where
  loop = do
    bs <- liftIO (NSB.recv sock nbytes)
    if B.null bs
      then return ()
      else yield bs >> loop

socketToProducerMaybe :: Socket -> Int -> Producer ByteString (MaybeT IO) a
socketToProducerMaybe sock nbytes = loop
  where
  loop = do
    bs <- liftIO (NSB.recv sock nbytes)
    if B.null bs
      then lift (MaybeT (return Nothing))
      else yield bs >> loop

socketToProducerEither :: Socket -> Int -> Producer ByteString (ExceptT TeleshellError IO) a
socketToProducerEither sock nbytes = loop
  where
  loop = do
    bs <- liftIO (NSB.recv sock nbytes)
    if B.null bs
      then lift (ExceptT (return (Left TeleshellErrorClosed)))
      else yield bs >> loop


socketToConsumer :: Socket -> Consumer ByteString IO r
socketToConsumer sock = for cat (\a -> lift (NSB.sendAll sock a))

socketToConsumerMaybe :: Socket -> Consumer ByteString (MaybeT IO) r
socketToConsumerMaybe sock = for cat (\a -> lift (lift (NSB.sendAll sock a)))

socketToConsumerEither :: Socket -> Consumer ByteString (ExceptT e IO) r
socketToConsumerEither sock = for cat (\a -> lift (lift (NSB.sendAll sock a)))

teleshell :: Monad m
  => Exchange
  -> Pipe ByteString ByteString (ExceptT TeleshellError m) LB.ByteString 
teleshell (Exchange cmd prompt) = do
  mechoed <- case cmd of
    CommandLine c -> yield c >> yield (BC.singleton '\n') >> return (Just c)
    CommandHidden c -> yield c >> yield (BC.singleton '\n') >> return (Just BC.empty)
    CommandEmpty -> return Nothing
  e <- consumeBreakSubstringDropBeginning mechoed prompt
  lb <- case e of
    Left err -> lift (ExceptT (return (Left (err cmd))))
    Right lb -> return lb
  return lb

consumeBreakSubstringDropBeginning :: Monad m
  => Maybe ByteString
  -> ByteString
  -> Consumer' ByteString m (Either (Command -> TeleshellError) LB.ByteString)
consumeBreakSubstringDropBeginning mechoed pat = do
  let echoed = fromMaybe B.empty mechoed
  e <- consumeDropExactLeftovers B.empty echoed
  case e of
    Left (ix,remaining) -> return $ Left
      $ TeleshellErrorExpectedEcho (B.take ix echoed <> remaining)
    Right leftovers1 -> do
      leftovers2 <- case mechoed of
        Just _ -> do
          leftoversTmp <- consumeDropWhileLeftovers leftovers1 (== '\r')
          consumeDropWhileLeftovers leftoversTmp (== '\n')
        Nothing -> return B.empty
      (bb, promptAndLeftovers3) <- consumeBreakSubstringLeftovers leftovers2 pat
      let lb = BB.toLazyByteString bb
          leftovers3 = B.drop (B.length pat) promptAndLeftovers3
      if B.null leftovers3
        then return (Right lb)
        else return (Left (TeleshellErrorLeftovers lb pat leftovers3))

data TeleshellError
  = TeleshellErrorExpectedEcho ByteString Command
  | TeleshellErrorLeftovers LB.ByteString ByteString ByteString Command
    -- ^ Consumed, matched prompt, remaining after matched prompt, command issued
  | TeleshellErrorClosed
  deriving (Show)

-- data TeleshellContextError = TeleshellContextError
--   { tceCommand :: !Command
--   , tceError :: !TeleshellError
--   } deriving (Show)

data Exchange = Exchange
  { exchangeCommand :: !Command
  , commandPrompt :: !ByteString
  } deriving (Show)

data Command
  = CommandLine ByteString
  | CommandHidden ByteString
  | CommandEmpty
  deriving (Show)

-- instance IsString Command where
--   fromString = CommandLine . fromString

