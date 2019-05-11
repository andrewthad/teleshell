{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}

module Teleshell
  ( -- * Types
    Command(..)
  , Exchange(..)
  , TeleshellError(..)
  , Timeout(..)
  
    -- * 
  , defaultTimeout
  , runEndpoint
  , teleshell  
  ) where

import Pipes.ByteString.Substring (consumeDropExactLeftovers,consumeDropWhileLeftovers,consumeBreakSubstringLeftovers)
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.String (IsString(fromString))
import Pipes
import Data.Maybe (fromMaybe)
import Socket.Stream.IPv4
import Control.Concurrent.STM.TVar
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as LB

data TeleshellError
  = TeleshellErrorExpectedEcho !ByteString !Command
  | TeleshellErrorLeftovers !ByteString !ByteString !ByteString !Command  
  | TeleshellErrorClosed !CloseException
  | TeleshellErrorReceiveException !(ReceiveException 'Interruptible)
  | TeleshellErrorSendException !(SendException 'Uninterruptible)
  | TeleshellErrorConnectionException !(ConnectException 'Uninterruptible)
  deriving stock (Eq, Ord, Show)

data Command
  = CommandLine !ByteString
    -- ^ A command followed by a newline. The server is expected to echo this command back.
  | CommandHidden !ByteString
    -- ^ A command followed by a newline. The server is not expected to echo the command back.
  | CommandEmpty
    -- ^ A command consisting of nothing, not even a newline. This is useful
    --   as an initial command because it forces consumption of any pre-command
    --   output the server has sent to the client.
  deriving stock (Eq, Ord, Show)

-- | @'fromString' "foo"@ = @'CommandLine' ('fromString' "foo")@
instance IsString Command where
  fromString = CommandLine . fromString

data Exchange = Exchange
  { exchangeCommand :: !Command
    -- ^ command to send to the remote host
  , exchangePrompt :: !ByteString
    -- ^ prompt we expect to see
  }
  deriving stock (Eq, Ord, Show)

connectionToConsumer :: Connection -> Consumer ByteString (ExceptT TeleshellError IO) r
connectionToConsumer c = for cat $ \b -> do
  e <- lift . lift $ sendByteString c b
  case e of
    Left s -> lift . ExceptT . pure . Left . TeleshellErrorSendException $ s
    Right () -> pure ()

connectionToProducer :: Connection -> Int -> Producer ByteString (ExceptT TeleshellError IO) r
connectionToProducer c nbytes = loop
  where
    loop = do
      ebs <- liftIO $ recvTimeout c nbytes
      case ebs of
        Left r -> lift . ExceptT . pure . Left $ r
        Right b -> if B.null b
          then lift . ExceptT . pure . Left $ TeleshellErrorClosed ClosePeerContinuedSending
          else yield b >> loop

-- | A timeout in microseconds.
newtype Timeout = Timeout { getTimeout :: Int }

defaultTimeout :: Timeout
defaultTimeout = Timeout 5000000

recvTimeout :: ()
  => Connection -- ^ connection
  -> Int -- ^ number of bytes
  -> IO (Either TeleshellError ByteString)
recvTimeout = recvTimeoutWith defaultTimeout

recvTimeoutWith :: ()
  => Timeout -- ^ timeout
  -> Connection -- ^ connection
  -> Int -- ^ number of bytes
  -> IO (Either TeleshellError ByteString)
recvTimeoutWith (Timeout t) c nbytes = do
  delay <- registerDelay t
  interruptibleReceiveBoundedByteStringSlice delay c nbytes 0 >>= \case
    Left r -> pure (Left (TeleshellErrorReceiveException r))
    Right b -> pure (Right b)

runEndpoint :: forall a. ()
  => Endpoint
  -> Pipe ByteString ByteString (ExceptT TeleshellError IO) a
  -> IO (Either TeleshellError a)
runEndpoint e p = do
  w <- withConnection e
         (\e' x -> case e' of
             Left c -> pure (Left (TeleshellErrorClosed c))
             Right () -> case x of
               Left t -> pure (Left t)
               Right a -> pure (Right a) 
         )
         (\c -> runExceptT
            $ runEffect
            $ connectionToProducer c 4096 >-> p >-> connectionToConsumer c
         )
  case w of
    Left e' -> pure (Left (TeleshellErrorConnectionException e'))
    Right x -> pure x

teleshell :: Monad m
  => Exchange
  -> Pipe ByteString ByteString (ExceptT TeleshellError m) ByteString
teleshell (Exchange cmd prompt) = do
  mechoed <- case cmd of
    CommandLine c -> do
      yield c
      yield (BC8.singleton '\n')
      pure (Just c)
    CommandHidden c -> do
      yield c
      yield (BC8.singleton '\n')
      pure (Just mempty)
    CommandEmpty -> do
      pure Nothing
  e <- consumeBreakSubstringDropBeginning mechoed prompt
  lb <- case e of
    Left err -> lift . ExceptT . pure . Left $ err cmd
    Right lb -> pure lb
  pure lb

consumeBreakSubstringDropBeginning :: Monad m
  => Maybe ByteString
  -> ByteString
  -> Consumer' ByteString m (Either (Command -> TeleshellError) ByteString)
consumeBreakSubstringDropBeginning mechoed pat = do
  let echoed = fromMaybe mempty mechoed
  e <- consumeDropExactLeftovers mempty echoed
  case e of
    Left (ix,remaining) -> pure
      $ Left
      $ TeleshellErrorExpectedEcho (B.take ix echoed <> remaining)
    Right leftovers1 -> do
      leftovers2 <- case mechoed of
        Just _ -> do
          leftoversTmp <- consumeDropWhileLeftovers leftovers1 (== '\r')
          consumeDropWhileLeftovers leftoversTmp (== '\n')
        Nothing -> pure mempty
      (bb,promptAndLeftovers3) <- consumeBreakSubstringLeftovers leftovers2 pat
      let lb = LB.toStrict (BB.toLazyByteString bb)
          leftovers3 = B.drop (B.length pat) promptAndLeftovers3
      pure $ if B.null leftovers3
        then Right lb
        else Left (TeleshellErrorLeftovers lb pat leftovers3)
