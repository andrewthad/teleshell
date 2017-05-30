{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Socket
import Network.Socket.ByteString as NSB
import Network.Teleshell
import Data.Monoid
import Pipes
import Pipes.Core
import Data.ByteString (ByteString)
import qualified Pipes.Prelude as PP
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString.Char8 as BC

remoteHost :: String
remoteHost = "10.109.74.139"

main :: IO ()
main = withSocketsDo $ do       
  addrinfos <- getAddrInfo Nothing (Just remoteHost) (Just "23")       
  let serveraddr = head addrinfos       
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol       
  connect sock (addrAddress serveraddr)       
  putStrLn "Connected"       
  e <- runSocketPipeEither sock $
    (teleshell >\\ (chainPull (lift . displaySent) (lift . displayReceived) +>> client))
    --     PP.chain (lift . displayTraffic "out")
    -- >-> (teleshell >\\ client)
    -- >-> PP.chain (lift . displayTraffic "in")
  case e of
    Right () -> putStrLn "Succeeded"
    Left err -> print err

chainPull :: Monad m => (a' -> m ()) -> (a -> m ()) -> a' -> Proxy a' a a' a m r
chainPull f g = go
  where
  go a' = do
    lift (f a')
    a <- request a'
    lift (g a)
    respond a >>= go

displaySent :: Exchange -> IO ()
displaySent (Exchange cmd prompt) = do
  BC.putStr $ "Sent (Expecting prompt [" <> prompt <> "]): "
  case cmd of
    CommandEmpty -> BC.putStrLn "no command sent"
    CommandLine c -> BC.putStrLn c
    CommandHidden c -> BC.putStrLn c

displayReceived :: LB.ByteString -> IO ()
displayReceived bs = do
  putStrLn "Received"
  putStrLn "========="
  LBC.putStrLn bs
  putStrLn "========="

displayTraffic :: String -> ByteString -> IO ()
displayTraffic name bs = do
  putStrLn name
  putStrLn "========="
  BC.putStrLn bs
  putStrLn "========="

client :: Monad m => Client' Exchange LB.ByteString m ()
client = do
  _ <- request (Exchange CommandEmpty "Login: ")
  _ <- request (Exchange (CommandLine "notrealusername") "Password: ")
  _ <- request (Exchange (CommandHidden "notrealpassword") "ZNID42xx-Router> ")
  _ <- request (Exchange (CommandLine "enable") "ZNID42xx-Router# ")
  _ <- request (Exchange (CommandLine "show system info") "ZNID42xx-Router# ")
  _ <- request (Exchange (CommandLine "show vlan all") "ZNID42xx-Router# ")
  _ <- request (Exchange (CommandLine "show interface rate-limit all") "ZNID42xx-Router# ")
  return ()


-- teleshell :: String -> Word16 -> ByteString -> Server ByteString Builder IO ()
-- teleshell host port bs0 = do
--   sock <- lift initialize
--   teleshellConnected sock
-- 
-- teleshellConnected :: Socket -> ByteString -> Server ByteString Builder IO ()
-- teleshellConnected sock = go
--   where
--   go :: ByteString -> Server ByteString Builder IO ()
--   go bs = do
--     (bb,leftovers) <- lift (runEffect (socketToProducer sock >-> consumeBreakSubstring))
--     if B.null leftovers
--       then respond bb >>= go
--       else lift (fail "teleshell: there should not be any leftovers")
-- 
-- socketToProducer
--   :: Socket
--   -> Int
--   -> Producer' B.ByteString IO ()
-- socketToProducer sock nbytes = loop
--   where
--   loop = do
--     bs <- liftIO (NSB.recv sock nbytes)
--     if B.null bs
--       then return ()
--       else yield bs >> loop
    

-- -- | The second element in the tuple returned in any additional
-- --   output that showed up after the prompt.
-- recvUntilPrompt :: ByteString -> Socket -> IO (LB.ByteString, ByteString)
-- recvUntilPrompt prompt s = go mempty Nothing
--   where
--   go :: BB.Builder -> Maybe ByteString -> IO (LB.ByteString, ByteString)
--   go b mremaining = do
--     chunk <- NSB.recv 4096
--     case mremaining of
--       Nothing -> 
--       Just remaining -> do
--         let remainingLen = B.length remaining
--             (chunkPre,chunkPost) = B.splitAt remainingLen chunk
--         if remainingLen > 
--     case breakSubstringCont prompt chunk of
--       Left mmatch -> 
--       Right (preprompt,promptOnward) -> do
--         let onward = B.drop (B.length prompt) promptOnward
--          in (b <> BB.fromStrict preprompt, onward)
-- 
-- untilPrompt :: forall m. Monad m => ByteString -> Consumer ByteString m (LB.ByteString, ByteString)
-- untilPrompt !prompt = go mempty Nothing
--   where
--   go1 :: BB.Builder -> Maybe Int -> Consumer ByteString m (LB.ByteString, ByteString)
--   go1 bb mmatchedPromptLen
--     chunk <- await
--     case mmatchedPromptLen of
--       Nothing -> 
--       Just matchedPromptLen -> do
--         let remPrompt = B.drop matchedPromptLen
--             remPromptLen = B.length remPrompt
--             chunkLen = B.length chunk
--         if remPromptLen > chunkLen
--           then do
--             let remainingPre = B.take chunkLen remPrompt
--             if chunk == remainingPre
--               then go1 bb (Just (chunkLen + matchedPromptLen))
--               else go1 bb (return (BB.toLazyByteString bb,chunkPost)
--           else do
--             let (chunkPre,chunkPost) = B.splitAt remainingLen chunk
--             if chunkPre == remaining
--               then return (BB.toLazyByteString bb,chunkPost)
--               else go2
--     case breakSubstringCont prompt chunk of
--       Left mmatch -> 
--       Right (preprompt,promptOnward) -> do
--         let onward = B.drop (B.length prompt) promptOnward
--          in (b <> BB.fromStrict preprompt, onward)
-- 
-- 
-- breakSubstringCont :: ByteString -> ByteString 
--   -> Either (Maybe ByteString) (ByteString,ByteString)
-- breakSubstringCont needle haystack = error "write me"

