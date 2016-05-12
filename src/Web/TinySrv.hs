{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Web.TinySrv (
    serve
  , serveSync
  , module Web.TinySrv.Monad
  , module Web.TinySrv.Types
) where

import Prelude.Unicode

import Web.TinySrv.Monad
import Web.TinySrv.ResponseCodes
import Web.TinySrv.Types

import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Network (withSocketsDo, listenOn, PortID(PortNumber), accept)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hClose, hFlush, hIsEOF)

import Control.Concurrent (forkIO, forkFinally)
import Control.Monad (forever)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)

import Data.Time.Clock
import Data.Time.Format

import Control.Exception.Base

-- | Runs the server in a new thread
serve ∷ Integer -- ^ Port
      → [Route Response] -- ^ Routes
      → IO ()
serve p rs = do
    waitVar ← newEmptyMVar
    forkFinally (serveSync p rs) (\_ → putMVar waitVar ())
    takeMVar waitVar

-- | Starts the server in the calling thread, you probably don't need this one
serveSync ∷ Integer -- ^ Port
            → [Route Response] -- ^ Routes
            → IO ()
serveSync p rs = withSocketsDo $ do
    sock ← listenOn ∘ PortNumber $ fromIntegral p
    forever $ do
        (h, _, _) ← accept sock
        forkIO $ respond h rs

--Handle incoming request
respond ∷ Handle → [Route Response] → IO ()
respond h rs = do
    req ← parseRequest <$> B.hGetLine h --TODO handle bad request
    case req of
        BadRequest → executeResponse h req (Response 400 ("<h1>Bad Request</h1>" ∷ B.ByteString)) []
        otherwise → do
            hs ← filter (≠ BadHeader) ∘ map parseHeader <$> getHeaders []
            r ← catch (runRoutes rs (req{reqHeaders=hs}, [])) (\e → return ∘ Just $ (Response 500 $ (B.concat ["<h2>An error occurred</h2><hr>", B.pack $ displayException (e ∷ SomeException)]), [Header "Content-Type" "text/html"]))
            case r of
                Just (rsp, hdrs) → executeResponse h req rsp hdrs
                Nothing → executeResponse h req (Response 404 ("<h2>Not Found</h2>" ∷ B.ByteString)) []
    where
        --Takes string in the format of "header name: header value" and spits out a Header value for it
        parseHeader ∷ B.ByteString → Header
        parseHeader s
            | B.length hf ≠ B.length s = Header hf (B.dropWhile (≡ ' ') $ B.dropWhile (≠ ' ') s)
            | otherwise = BadHeader
            where
                hf = B.takeWhile (≠ ':') s
        --Take until empty line or EOF, returns lines in reverse order, since order doesn't matter for headers
        getHeaders ∷ [B.ByteString] → IO [B.ByteString]
        getHeaders hs = do
            eof ← hIsEOF h
            if not eof then do
                l ← B.hGetLine h
                case l of
                    "\r" → return hs
                    _  → getHeaders $ B.init l : hs
            else
                return hs

--Send response and headers
executeResponse ∷ Handle → Request → Response → [Header] → IO ()
executeResponse h r (Response c b) hs = do
    hSetBuffering h NoBuffering
    B.hPut h "HTTP/1.1 "
    B.hPut h ∘ B.pack $ show c
    B.hPut h $ lookupCode c
    B.hPut h "\r\n"
    t ← B.pack ∘ flip (++) "GMT"  ∘ dropWhileEnd (≠ ' ') ∘ formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
    ifNotHeader "Date" $ B.concat ["Date: ", t ,"\r\n"]
    ifNotHeader "Server" "Server: tinysrv\r\n"
    ifNotHeader "Content-Length" $ B.concat ["Content-Length: ", B.pack ∘ show $ contentLength b, "\r\n"]
    ifNotHeader "Content-Type" "Content-Type: text/html\r\n"
    mapM_ (B.hPut h ∘ (\(Header n v) → B.concat [n, ": ", v, "\r\n"])) hs
    B.hPut h "\r\n"
    writeStream h b
    if reqMethod r ≠ HEAD
        then writeStream h b >> hFlush h
        else return ()
    hClose h
    where
        ifNotHeader ∷ B.ByteString → B.ByteString → IO ()
        ifNotHeader n s = if null $ filter (\(Header n' _) → n' ≡ n) hs then B.hPut h s else return ()

--Parse the top line of the HTTP request
parseRequest ∷ B.ByteString → Request
parseRequest s
    | isValidMethod methodStr ∧ B.head path ≡ '/' = Request method file args undefined
    | otherwise = BadRequest
    where
        isValidMethod ∷ B.ByteString → Bool
        isValidMethod "GET"     = True
        isValidMethod "HEAD"    = True
        isValidMethod "POST"    = True
        isValidMethod "PUT"     = True
        isValidMethod "CONNECT" = True
        isValidMethod "TRACE"   = True
        isValidMethod "DELETE"  = True
        isValidMethod _         = False

        methodStr = B.takeWhile (≠ ' ') s

        method ∷ HTTPMethod
        method = case methodStr of
                     "GET"     → GET
                     "HEAD"    → HEAD
                     "POST"    → POST
                     "PUT"     → PUT
                     "CONNECT" → CONNECT
                     "TRACE"   → TRACE
                     "DELETE"  → DELETE

        path = B.takeWhile (≠ ' ') ∘ B.drop 1 $ B.dropWhile (≠ ' ') s
        (file, args) = processArgs (B.split '?' path)
        
        makeTuple ∷ [B.ByteString] → (B.ByteString, B.ByteString)
        makeTuple [x]    = (x, B.empty)
        makeTuple [x, y] = (x, y)

        processArgs ∷ [B.ByteString] → ([B.ByteString], [(B.ByteString, B.ByteString)])
        processArgs [f]     = (filter (≠ B.empty) $ B.split '/' f, [])
        processArgs [f, as] = (filter (≠ B.empty) $ B.split '/' f, map (makeTuple ∘ B.split '=') $ B.split '&' as)
