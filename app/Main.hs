{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (isPrefixOf)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)

newtype AppState = AppState {directory :: String}

data HttpMethod
  = HttpMethodGet
  | HttpMethodPost
  | HttpMethodUnknown
  deriving (Eq)

crlf :: BC.ByteString
crlf = "\r\n"

splitBy :: BC.ByteString -> BC.ByteString -> [BC.ByteString]
splitBy _ "" = []
splitBy sep bs = line : splitBy sep (BC.drop (BC.length sep) rest)
  where
    (line, rest) = BC.breakSubstring crlf bs

parseHttpMethod :: BC.ByteString -> HttpMethod
parseHttpMethod "GET" = HttpMethodGet
parseHttpMethod "POST" = HttpMethodPost
parseHttpMethod method = error $ "Invalid http method: " <> BC.unpack method

instance Show HttpMethod where
  show = BC.unpack . showHttpMethod

showHttpMethod :: HttpMethod -> BC.ByteString
showHttpMethod HttpMethodGet = "GET"
showHttpMethod HttpMethodPost = "POST"
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

data HttpStatus = HttpStatusOk | HttpStatusCreated | HttpStatusNotFound

showHttpStatus :: HttpStatus -> BC.ByteString
showHttpStatus HttpStatusOk = "200 OK"
showHttpStatus HttpStatusCreated = "201 Created"
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

getHttpHeader :: [HttpHeader] -> BC.ByteString -> HttpHeader
getHttpHeader [] _ = ("", "")
getHttpHeader headers key
  | fst header == key = header
  | otherwise = getHttpHeader (tail headers) key
  where
    header = head headers

withContentLength :: AppState -> (AppState -> HttpRequest -> IO HttpResponse) -> HttpRequest -> IO HttpResponse
withContentLength state handler req = do
  (status, headers, body) <- handler state req
  let contentLength = ("Content-Length", BLC.toStrict . BB.toLazyByteString . BB.intDec $ BC.length body)
  return (status, contentLength : headers, body)

handleRoute :: AppState -> HttpRequest -> IO HttpResponse
handleRoute state ((method, path, version), headers, body)
  | method == HttpMethodGet && [""] `isPrefixOf` path = return ((version, HttpStatusOk), [], "")
  | method == HttpMethodGet && ["echo"] `isPrefixOf` path = return ((version, HttpStatusOk), [textPlain], BC.intercalate "/" $ tail path)
  | method == HttpMethodGet && ["user-agent"] `isPrefixOf` path = return ((version, HttpStatusOk), [textPlain], snd $ getHttpHeader headers "User-Agent")
  | method == HttpMethodGet && ["files"] `isPrefixOf` path = do
      file <- getFileContent state . BC.intercalate "/" $ tail path
      return $ case file of
        Just content -> ((version, HttpStatusOk), [octetStream], content)
        Nothing -> ((version, HttpStatusNotFound), [], "")
  | method == HttpMethodPost && ["files"] `isPrefixOf` path = do
      writeContentToFile state (BC.intercalate "/" $ tail path) body
      return ((version, HttpStatusCreated), [octetStream], "")
  | otherwise = return ((version, HttpStatusNotFound), [], "")
  where
    textPlain = ("Content-Type", "text/plain")
    octetStream = ("Content-Type", "application/octet-stream")

parseRequestStatus :: [BC.ByteString] -> HttpRequestStatus
parseRequestStatus [method, path, version] = (parseHttpMethod method, tail $ BC.split '/' path, parseHttpVersion version)
parseRequestStatus [] = error "Invalid http request status"
parseRequestStatus n = error $ "Invalid http request status line: " <> BC.unpack (BC.unwords n)

parseRequestHeader :: [BC.ByteString] -> HttpHeader
parseRequestHeader (key : rest) = (BC.init key, BC.unwords rest)
parseRequestHeader [] = error "Invalid http request header"

parseRequest :: (HttpRequest, Bool) -> BC.ByteString -> (HttpRequest, Bool)
parseRequest (req@(status@(method, _, _), headers, body), found) line
  | not found && line == "" = (req, not found)
  | method == HttpMethodUnknown = ((parseRequestStatus $ BC.words line, headers, body), found)
  | not found = ((status, parseRequestHeader (BC.words line) : headers, body), found)
  | found = ((status, headers, (if BC.null body then body else body <> crlf) <> line), found)
  | otherwise = error $ "Invalid request line: " <> BC.unpack line

handleRequest :: AppState -> BC.ByteString -> IO BC.ByteString
handleRequest state reqStr = do
  let (req, _) = foldl parseRequest (((HttpMethodUnknown, [], HttpVersionUnknown), [], ""), False) $ splitBy crlf reqStr
  ((resVersion, resStatus), resHeaders, resBody) <- withContentLength state handleRoute req
  return $ BC.intercalate crlf [BC.unwords [showHttpVersion resVersion, showHttpStatus resStatus], BC.intercalate crlf (map concatHeader resHeaders), "", resBody]

handleConnection :: AppState -> Socket -> IO ()
handleConnection state soc = do
  req <- recv soc 1024
  res <- handleRequest state req
  _ <- send soc res
  close soc

getFileContent :: AppState -> BC.ByteString -> IO (Maybe BC.ByteString)
getFileContent state path = do
  contents <- try (BC.readFile $ directory state <> "/" <> BC.unpack path) :: IO (Either SomeException BC.ByteString)
  return $ case contents of
    Left _ -> Nothing
    Right content -> Just content

writeContentToFile :: AppState -> BC.ByteString -> BC.ByteString -> IO ()
writeContentToFile state path content = do
  BC.writeFile (directory state <> "/" <> BC.unpack path) content

main :: IO ()
main = do
  args <- getArgs

  let state =
        AppState
          { directory = if length args >= 2 && head args == "--directory" then args !! 1 else ""
          }

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
    forkIO $ handleConnection state clientSocket
