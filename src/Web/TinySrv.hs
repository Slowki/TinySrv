{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Web.TinySrv (
    serve
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
import System.IO (hClose, hFlush, Handle, hIsEOF)

import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Data.Time.Clock
import Data.Time.Format

-- | Start the server
serve ∷ Integer -- ^ Port
      → [Route Response] -- ^ Routes
      → IO ()
serve p rs = withSocketsDo $ do 
    sock ← listenOn ∘ PortNumber $ fromIntegral p
    forever $ do
        (h, _, _) ← accept sock
        forkIO $ respond h rs --TODO make a more efficient system

--Handle incoming request
respond ∷ Handle → [Route Response] → IO ()
respond h rs = do
    r ← parseRequest <$> B.hGetLine h --TODO handle bad request
    case r of
        BadRequest → executeResponse h (Response 400 ("<h1>Bad Request</h1>" ∷ B.ByteString)) []
        otherwise → do
            hs ← filter (≠ BadHeader) ∘ map parseHeader <$> getHeaders []
            r ← runRoutes rs (r{reqHeaders=hs}, [])
            case r of
                Just (rsp, hdrs) → executeResponse h rsp hdrs
                Nothing → executeResponse h (Response 404 ("<h1>Not Found</h1>" ∷ B.ByteString)) []
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
executeResponse ∷ Handle → Response → [Header] → IO ()
executeResponse h (Response c b) hs = do
    B.hPut h "HTTP/1.1 "
    B.hPut h (B.pack $ show c)
    B.hPut h $ lookupCode c
    B.hPut h "\r\n"
    t ← B.pack ∘ flip (++) "GMT"  ∘ dropWhileEnd (≠ ' ') ∘ formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
    ifNotHeader "Date" ∘ B.hPut h $ B.concat ["Date: ", t ,"\r\n"]
    ifNotHeader "Server" $ B.hPut h "Server: tinysrv\r\n"
    ifNotHeader "Content-Length" ∘ B.hPut h $ B.concat ["Content-Length: ", B.pack ∘ show $ streamLength b, "\r\n"]
    mapM_ (B.hPut h ∘ (\(Header n v) → B.concat [n, ": ", v, "\r\n"])) hs
    B.hPut h "\r\n"
    writeStream h b
    hFlush h
    hClose h
    where
        ifNotHeader ∷ B.ByteString → IO () → IO ()
        ifNotHeader n i = if null $ filter (\(Header n' _) → n' ≡ n) hs then i else return ()

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
