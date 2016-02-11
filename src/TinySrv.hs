{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module TinySrv (
    serve
  , module TinySrv.Monad
) where

import Prelude.Unicode

import TinySrv.Monad
import TinySrv.ResponseCodes

import Data.List
import qualified Data.ByteString.Char8 as B

import Network (withSocketsDo, listenOn, PortID(PortNumber), accept)
import System.IO (hClose, Handle, hIsEOF)

import Control.Concurrent

import Control.Monad (forever)

serve ∷ ℤ → [Route Response] → IO ()
serve p rs = withSocketsDo $ do 
    sock ← listenOn ∘ PortNumber $ fromIntegral p
    forever $ do
        (h,hn,_) ← accept sock
        forkIO $ respond h rs --TODO make a more efficient system

respond ∷ Handle → [Route Response] → IO ()
respond h rs = do
    r ← parseRequest <$> B.hGetLine h --TODO handle bad request
    case r of
        BadRequest → executeResponse h (Response 400 "<h1>Bad Request</h1>") []
        otherwise → do
            hs ← filter (≠ BadHeader) ∘ map parseHeader <$> getHeaders []
            case runRoutes rs (r{reqHeaders=hs}, []) of
                Just (rsp, hdrs) → executeResponse h rsp hdrs
                Nothing → executeResponse h (Response 404 "<h1>Not Found</h1>") []
    where
        --Takes string in the format of "header name: header value" and spits out a Header value for it
        parseHeader ∷ B.ByteString → Header
        parseHeader s
            | B.length hf ≠ B.length s = Header hf (B.dropWhile (≡ ' ') $ B.dropWhile (≠ ' ') s)
            | otherwise = BadHeader
            where
                hf = B.takeWhile (≠ ':') s
        --Take untill empty line or EOF, returns lines in reverse order, since order doesn't matter for headers
        getHeaders ∷ [B.ByteString] → IO [B.ByteString]
        getHeaders hs = do
            eof ← hIsEOF h
            if not eof then do
                l ← B.hGetLine h
                case l of
                    "\r" → return hs
                    _  → getHeaders $ (B.init l):hs
            else
                return hs

executeResponse ∷ Handle → Response → [Header] → IO ()
executeResponse h (Response c b) hs = do
    B.hPut h "HTTP/1.1 "
    B.hPut h (B.pack $ show c)
    B.hPut h $ lookupCode c
    B.hPut h "\r\n"
    B.hPut h $ B.concat ["Content-Length: ", B.pack ∘ show $ B.length b, "\r\n"]
    mapM_ (B.hPut h) $ map (\(Header n v) → B.concat [n, ": ", v, "\r\n"]) hs
    B.hPut h "\r\n"
    B.hPut h b
    hClose h

parseRequest ∷ B.ByteString → Request
parseRequest s
    | isValidMethod method ∧ B.head path ≡ '/' = Request method file args undefined
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

        method = B.takeWhile (≠ ' ') s
        path = B.takeWhile (≠ ' ') ∘ B.drop 1 $ B.dropWhile (≠ ' ') s
        (file, args) = processArgs (B.split '?' path)
        
        makeTuple ∷ [B.ByteString] → (B.ByteString, B.ByteString)
        makeTuple (x:[]) = (x, B.empty)
        makeTuple (x:y:[]) = (x, y)

        processArgs ∷ [B.ByteString] → ([B.ByteString], [(B.ByteString, B.ByteString)])
        processArgs (f:[]) = (filter (≠ B.empty) $ B.split '/' f, [])
        processArgs (f:as:[]) = (filter (≠ B.empty) $ B.split '/' f, map (makeTuple ∘ B.split '=') $ B.split '&' as)