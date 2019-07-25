{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}

module Teleshell
  ( -- * Types
    Command(..)
  , Exchange(..)
  , TeleshellError(..)
  , Timeout(..)

    -- * Functions
  , runEndpoint
  , teleshell
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Pipes
import Pipes.ByteString.Substring (consumeDropExactLeftovers,consumeDropWhileLeftovers,consumeBreakSubstringLeftovers)
import Socket.Stream.IPv4 (Family(Internet),Version(V4))
import Socket.Stream.IPv4 (CloseException,ReceiveException,ConnectException)
import Socket.Stream.IPv4 (Peer,Connection,Interruptibility(..),SendException)
import System.IO (Handle,hFlush)

import qualified Socket.Stream.IPv4 as S
import qualified Socket.Stream.Uninterruptible.ByteString as SU
import qualified Socket.Stream.Interruptible.ByteString as SI
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as LB

-- | The type of errors we can encounter when interacting with a telnet
--   server.
data TeleshellError
  = TeleshellErrorExpectedEcho !ByteString !Command
  | TeleshellErrorLeftovers !ByteString !ByteString !ByteString !Command
  | TeleshellErrorClosed !CloseException
  | TeleshellErrorReceiveException !(ReceiveException 'Interruptible)
  | TeleshellErrorSendException !(SendException 'Uninterruptible)
  | TeleshellErrorConnectionException !(ConnectException ('Internet 'V4) 'Uninterruptible)
  deriving stock (Eq, Ord, Show)

-- | A command to be sent to a telnet server.
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

-- | An exchange is a command and the response we expect to see
--   as a result of that command.
data Exchange = Exchange
  { exchangeCommand :: !Command
    -- ^ command to send to the remote host
  , exchangePrompt :: !ByteString
    -- ^ prompt we expect to see
  }
  deriving stock (Eq, Ord, Show)

connectionToConsumer :: ()
  => Handle
  -> Connection
  -> Consumer ByteString (ExceptT TeleshellError IO) r
connectionToConsumer h c = for cat $ \b -> do
  liftIO $ hPut h b
  e <- lift . lift $ SU.send c b
  case e of
    Left s -> lift . ExceptT . pure . Left . TeleshellErrorSendException $ s
    Right () -> pure ()

connectionToProducer :: ()
  => Handle -- ^ handle to which we log recv messages
  -> Timeout -- ^ number of microseconds after which we should timeout
  -> Connection
  -> Int
  -> Producer ByteString (ExceptT TeleshellError IO) r
connectionToProducer h t c nbytes = loop
  where
    loop = do
      ebs <- liftIO $ recvTimeout h t c nbytes
      case ebs of
        Left r -> lift . ExceptT . pure . Left $ r
        Right b -> yield b >> loop

-- | A timeout in microseconds.
newtype Timeout = Timeout { getTimeout :: Int }

recvTimeout :: ()
  => Handle -- ^ handle to which recv messages will be logged
  -> Timeout -- ^ timeout
  -> Connection -- ^ connection
  -> Int -- ^ number of bytes
  -> IO (Either TeleshellError ByteString)
recvTimeout h (Timeout t) c nbytes = do
  delay <- registerDelay t
  SI.receiveOnce delay c nbytes >>= \case
    Left r -> pure (Left (TeleshellErrorReceiveException r))
    Right b -> do
      hPut h b
      pure (Right b)

-- | Connect to a 'Peer' where a telnetd server is running.
runEndpoint :: forall a. ()
  => Handle -- ^ Handle to which we should log recv messages
  -> Handle -- ^ Handle to which we should log send messages
  -> Timeout
  -> Peer
  -> Pipe ByteString ByteString (ExceptT TeleshellError IO) a
  -> IO (Either TeleshellError a)
runEndpoint hRecv hSend t e p = do
  w <- S.withConnection e
         (\e' x -> case e' of
             Left c -> pure (Left (TeleshellErrorClosed c))
             Right () -> case x of
               Left t -> pure (Left t)
               Right a -> pure (Right a)
         )
         (\c -> runExceptT
            $ runEffect
            $ connectionToProducer hRecv t c 4096 >-> p >-> connectionToConsumer hSend c
         )
  case w of
    Left e' -> pure (Left (TeleshellErrorConnectionException e'))
    Right x -> pure x

teleshell :: ()
  => Exchange
  -> Pipe ByteString ByteString (ExceptT TeleshellError IO) ByteString
teleshell (Exchange cmd prompt) = do
  mechoed <- case cmd of
    CommandLine c -> do
      let msg = c <> "\n"
      yield msg
      pure (Just c)
    CommandHidden c -> do
      let msg = c <> "\n"
      yield msg
      pure (Just mempty)
    CommandEmpty -> do
      pure Nothing
  e <- consumeBreakSubstringDropBeginning mechoed prompt
  case e of
    Left err -> lift . ExceptT . pure . Left $ err cmd
    Right lb -> pure lb

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

-- Make sure to flush after each hPut, to guarantee sending of messages
-- to the handle
hPut :: Handle -> ByteString -> IO ()
hPut h b = do
  B.hPut h b
  hFlush h
