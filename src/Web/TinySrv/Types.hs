{-# LANGUAGE UnicodeSyntax, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
module Web.TinySrv.Types (
      HTTPMethod(..)
    , Header(..)
    , Request(..)
    , Response(..)
    , HPutable
    , writeStream
    , contentLength
    ) where

import Prelude.Unicode

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B (hPut, hGetSome, length)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, hPut, length)
import qualified Data.Text as T (Text, length)
import qualified Data.Text.IO as T (hPutStr)
import System.IO (Handle, hPutStr, hIsEOF)

data HTTPMethod = GET
                | HEAD
                | POST
                | PUT
                | CONNECT
                | TRACE
                | DELETE
                deriving (Show, Read, Eq)

data Header = Header {-# UNPACK #-} !ByteString {-# UNPACK #-} !ByteString | BadHeader
    deriving (Show, Read, Eq)

data Request =
    Request {
              reqMethod ∷ HTTPMethod
            , reqPath ∷ [ByteString]
            , reqArgs ∷ [(ByteString, ByteString)]
            , reqHeaders ∷ [Header]
            }
            | BadRequest
    deriving Show

data Response = ∀ a. HPutable a ⇒ Response {-# UNPACK #-} !Int a

class HPutable a where
    writeStream  ∷ Handle → a → IO () -- | Usually just hPut, or a conversion to ByteString then hPut
    contentLength ∷ a → Int            -- | Should return the length of the stream in bytes

instance HPutable ByteString where
    writeStream  = B.hPut
    contentLength = B.length

instance HPutable BL.ByteString where
    writeStream  = BL.hPut
    contentLength = fromIntegral ∘ BL.length

instance HPutable T.Text where
    writeStream  = T.hPutStr
    contentLength = T.length

instance HPutable String where
    writeStream  = hPutStr
    contentLength = length

instance HPutable (Integer, Handle) where
    writeStream oh (l, ih) = do
        e ← hIsEOF ih
        if e
            then return ()
            else B.hGetSome ih 128 >>= B.hPut oh >> writeStream oh (l, ih)
    contentLength (l, h) = fromIntegral l