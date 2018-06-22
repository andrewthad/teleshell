{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

module Network.Teleshell
  ( runSocketPipe
  , runSocketPipeMaybe
  , runSocketPipeEither
  , teleshell
  , Exchange(..)
  , Command(..)
  , TeleshellError(..) 
  , Timeout(..) 
  , defaultTimeout 
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.Functor
import Data.Maybe
import Data.Semigroup
import Data.String
import GHC.Conc.IO (threadWaitReadSTM)
import Network
import Pipes
import Pipes.ByteString.Substring (consumeBreakSubstringLeftovers,consumeDropExactLeftovers,consumeDropWhileLeftovers)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket as NS
import qualified System.Posix.Types

-- | Type wrapping an 'Int' that is interpreted
-- as a timeout interval in microseconds.
newtype Timeout = Timeout Int

defaultTimeout :: Timeout
defaultTimeout = Timeout 5000000

-- | The socket must already be connected.
runSocketPipe :: Socket -> Pipe ByteString ByteString IO () -> IO ()
runSocketPipe sock p = do
  runEffect (socketToProducer sock 4096 >-> p >-> socketToConsumer sock)

-- | The socket must already be connected.
runSocketPipeMaybe :: Socket -> Pipe ByteString ByteString (MaybeT IO) a -> IO (Maybe a)
runSocketPipeMaybe sock p = do
  runMaybeT (runEffect (socketToProducerMaybe sock 4096 >-> p >-> socketToConsumerMaybe sock))

-- | The socket must already be connected. The upstream and downstream
--   are provided by TCP @recv@ and @send@, respectively. More specifically,
--
--   * Every 'await' is replaced by 'Network.Socket.ByteString.recv'
--   * Every 'yield' is replaced by 'Network.Socket.ByteString.sendAll'
--
--   This is an unusual way to use the machinery provided by @pipes@.
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
    bs <- lift $ ExceptT $ recvTimeout sock nbytes
    if B.null bs
      then lift (ExceptT (pure (Left TeleshellErrorClosed)))
      else yield bs >> loop

recvTimeout
  :: Socket
  -> Int
  -> IO (Either TeleshellError ByteString)
recvTimeout = recvTimeoutWith defaultTimeout

recvTimeoutWith
  :: Timeout
  -> Socket
  -> Int
  -> IO (Either TeleshellError ByteString)
recvTimeoutWith (Timeout t) sock nbytes = do
  (isReadyAction,deregister) <- threadWaitReadSTM (socketToFd sock)
  delay <- STM.registerDelay t
  isContentReady <- STM.atomically $ (isReadyAction $> True) <|> (fini delay $> False)
  deregister
  if isContentReady
    then do
      msg <- NSB.recv sock nbytes
      putStrLn $ BC.unpack msg
      pure $ Right msg
    else pure $ Left TeleshellErrorTimeout  
  where
    socketToFd :: NS.Socket -> System.Posix.Types.Fd
    socketToFd (NS.MkSocket n _ _ _ _) = System.Posix.Types.Fd n
    fini :: STM.TVar Bool -> STM.STM ()
    fini = STM.check <=< STM.readTVar

socketToConsumer :: Socket -> Consumer ByteString IO r
socketToConsumer sock = for cat (\a -> lift (NSB.sendAll sock a))

socketToConsumerMaybe :: Socket -> Consumer ByteString (MaybeT IO) r
socketToConsumerMaybe sock = for cat (\a -> lift (lift (NSB.sendAll sock a)))

socketToConsumerEither :: Socket -> Consumer ByteString (ExceptT e IO) r
socketToConsumerEither sock = for cat (\a -> lift (lift (NSB.sendAll sock a)))

-- | A pipe intended to be composed with a 'Client' to allow it to be
--   plugged in to 'runSocketPipeEither'. For example:
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.ByteString.Lazy as BL 
-- >>> :{
-- let myClient :: Monad m => Client' Exchange LB.ByteString m Int
--     myClient = do
--       sysStr <- request (Exchange (CommandLine "show system info") "XKB-9212> ")
--       vlanStr <- request (Exchange (CommandLine "show vlan all") "XKB-9212> ")
--       return 55
-- :}
--
--   We can then connect this to a socket with an operator from @Pipes.Core@:
--
-- >>> let myPipe = teleshell >\\ myClient
-- >>> :t myPipe
-- myPipe
--   :: Monad m =>
--      Proxy () ByteString () ByteString (ExceptT TeleshellError m) Int
-- 
-- Note that this type is just the expansion of the type synonym @Pipe@. Finally,
-- we can run this by providing an already-connected socket:
--
-- >>> let program = runSocketPipeEither (error "provide a socket please") myPipe
-- >>> :t program
-- program :: IO (Either TeleshellError Int)
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
  | TeleshellErrorTimeout 
  deriving (Show)

data Exchange = Exchange
  { exchangeCommand :: !Command
    -- ^ command to send to the remote host
  , exchangePrompt :: !ByteString
    -- ^ prompt we expect to see
  } deriving (Show)

data Command
  = CommandLine ByteString
    -- ^ A command followed by a newline. The server is expected to echo this command back.
  | CommandHidden ByteString
    -- ^ A command followed by a newline. The server is not expected to echo the command back.
  | CommandEmpty
    -- ^ A command consisting of nothing, not even a newline. This is useful
    --   as an initial command because it forces consumption of any pre-command
    --   output the server has sent to the client.
  deriving (Show)

instance IsString Command where
  fromString = CommandLine . fromString
