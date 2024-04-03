{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (isPrefixOf)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.IO (BufferMode (..), hSetBuffering, stdout)

data HttpMethod = HttpMethodGet | HttpMethodUnknown deriving (Eq)

crlf :: BC.ByteString
crlf = "\r\n"

splitBy :: BC.ByteString -> BC.ByteString -> [BC.ByteString]
splitBy _ "" = []
splitBy sep bs = line : splitBy sep (BC.drop (BC.length sep) rest)
  where
    (line, rest) = BC.breakSubstring crlf bs

parseHttpMethod :: BC.ByteString -> HttpMethod
parseHttpMethod "GET" = HttpMethodGet
parseHttpMethod method = error $ "Invalid http method: " <> BC.unpack method

instance Show HttpMethod where
  show = BC.unpack . showHttpMethod

showHttpMethod :: HttpMethod -> BC.ByteString
showHttpMethod HttpMethodGet = "GET"
showHttpMethod HttpMethodUnknown = "UNKNOWN"

data HttpVersion = HttpVersion1_1 | HttpVersionUnknown

parseHttpVersion :: BC.ByteString -> HttpVersion
parseHttpVersion "HTTP/1.1" = HttpVersion1_1
parseHttpVersion version = error $ "Invalid http version: " <> BC.unpack version

showHttpVersion :: HttpVersion -> BC.ByteString
showHttpVersion HttpVersion1_1 = "HTTP/1.1"
showHttpVersion HttpVersionUnknown = error "Cannot show the unknown http version"

instance Show HttpVersion where
  show = BC.unpack . showHttpVersion

data HttpStatus = HttpStatusOk | HttpStatusNotFound

showHttpStatus :: HttpStatus -> BC.ByteString
showHttpStatus HttpStatusOk = "200 OK"
showHttpStatus HttpStatusNotFound = "404 Not Found"

instance Show HttpStatus where
  show = BC.unpack . showHttpStatus

type HttpBody = BC.ByteString

type HttpHeader = (BC.ByteString, BC.ByteString)

type HttpRequestStatus = (HttpMethod, [BC.ByteString], HttpVersion)

type HttpRequest = (HttpRequestStatus, [HttpHeader], HttpBody)

type HttpResponseStatus = (HttpVersion, HttpStatus)

type HttpResponse = (HttpResponseStatus, [HttpHeader], HttpBody)

concatHeader :: (BC.ByteString, BC.ByteString) -> BC.ByteString
concatHeader (key, value) = key <> ": " <> value

getHeader :: [HttpHeader] -> BC.ByteString -> HttpHeader
getHeader [] _ = ("", "")
getHeader headers key
  | fst header == key = header
  | otherwise = getHeader (tail headers) key
  where
    header = head headers

withContentLength :: (HttpRequest -> HttpResponse) -> HttpRequest -> HttpResponse
withContentLength handler req = case handler req of
  (status, headers, body) -> (status, contentLength : headers, body)
    where
      contentLength = ("Content-Length", BLC.toStrict . BB.toLazyByteString . BB.intDec $ BC.length body)

handleRoute :: HttpRequest -> HttpResponse
handleRoute ((method, path, version), headers, _)
  | method == HttpMethodGet && [""] `isPrefixOf` path = ((version, HttpStatusOk), [], "")
  | method == HttpMethodGet && ["echo"] `isPrefixOf` path = ((version, HttpStatusOk), [contentType], BC.intercalate "/" $ tail path)
  | method == HttpMethodGet && ["user-agent"] `isPrefixOf` path = ((version, HttpStatusOk), [contentType], snd $ getHeader headers "User-Agent")
  | otherwise = ((version, HttpStatusNotFound), [], "")
  where
    contentType = ("Content-Type", "text/plain")

parseRequestStatus :: [BC.ByteString] -> HttpRequestStatus
parseRequestStatus [method, path, version] = (parseHttpMethod method, tail $ BC.split '/' path, parseHttpVersion version)
parseRequestStatus [] = error "Invalid http request status"
parseRequestStatus n = error $ "Invalid http request status line: " <> BC.unpack (BC.unwords n)

parseRequestHeader :: [BC.ByteString] -> HttpHeader
parseRequestHeader (key : rest) = (BC.init key, BC.unwords rest)
parseRequestHeader [] = error "Invalid http request header"

parseRequest :: (HttpRequest, Bool) -> BC.ByteString -> (HttpRequest, Bool)
parseRequest (req@(status@(method, _, _), headers, body), found) line
  | line == "" = (req, True)
  | method == HttpMethodUnknown = ((parseRequestStatus $ BC.words line, headers, body), found)
  | not found = ((status, parseRequestHeader (BC.words line) : headers, body), found)
  | found = ((status, headers, body <> crlf <> line), found)
  | otherwise = error $ "Invalid request line: " <> BC.unpack line

handleRequest :: BC.ByteString -> BC.ByteString
handleRequest reqStr =
  let (req, _) = foldl parseRequest (((HttpMethodUnknown, [], HttpVersionUnknown), [], ""), False) $ splitBy crlf reqStr
      ((resVersion, resStatus), resHeaders, resBody) = withContentLength handleRoute req
   in BC.intercalate crlf [BC.unwords [showHttpVersion resVersion, showHttpStatus resStatus], BC.intercalate crlf (map concatHeader resHeaders), "", resBody]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let host = "127.0.0.1"
      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  setSocketOption serverSocket ReuseAddr 1
  withFdSocket serverSocket setCloseOnExecIfNeeded
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr)

    req <- recv clientSocket 1024
    let res = handleRequest req
    _ <- send clientSocket res

    close clientSocket
