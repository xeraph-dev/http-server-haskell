{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.IO (BufferMode (..), hSetBuffering, stdout)

concatHeader :: (BC.ByteString, BC.ByteString) -> BC.ByteString
concatHeader (key, value) = key <> ": " <> value

handleRoute :: BC.ByteString -> [BC.ByteString] -> (BC.ByteString, [(BC.ByteString, BC.ByteString)], BC.ByteString)
handleRoute "GET" [""] = ("200 OK", [], "")
handleRoute "GET" ("echo" : path) = ("200 OK", [contentType, contentLength], body)
  where
    body = BC.intercalate "/" path
    contentType = ("Content-Type", "text/plain")
    contentLength = ("Content-Length", BLC.toStrict . BB.toLazyByteString . BB.intDec $ BC.length body)
handleRoute _ _ = ("404 Not Found", [], "")

handleRequest :: BC.ByteString -> IO BC.ByteString
handleRequest req = do
  let requestLines = BC.lines req
      statusLine = BC.words $ head requestLines
      method = head statusLine
      path = tail $ BC.split '/' $ statusLine !! 1
      version = statusLine !! 2
      (resStatusCode, resHeaders, resBody) = handleRoute method path

  return $ BC.intercalate "\r\n" [BC.intercalate " " [version, resStatusCode], BC.intercalate "\r\n" (map concatHeader resHeaders), "", resBody]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let host = "127.0.0.1"
      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  -- Get address information for the given host and port
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  setSocketOption serverSocket ReuseAddr 1
  withFdSocket serverSocket setCloseOnExecIfNeeded
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  -- Accept connections and handle them forever
  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr)

    _ <- recv clientSocket 1024 >>= handleRequest >>= send clientSocket

    close clientSocket
