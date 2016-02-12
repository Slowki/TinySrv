{-# LANGUAGE UnicodeSyntax #-}
module TinySrv.Monad (
      Route
    , Request(..)
    , Header(..)
    , Response(..)
    , okay
    , okayL
    , notFound
    , notFoundL
    --, serveFile
    , header
    , contentType
    , pathList
    , fullPath
    , path
    , popPath
    , emptyPath
    , headerList
    , getHeader
    , host
    , runRoutes
) where

import Prelude.Unicode

import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B (ByteString, pack, split, concat)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, readFile)
import Control.Monad (msum)
import Data.List (null)

type MaybeState s a = MaybeT (StateT s IO) a

runMaybeState ∷ MaybeState s a → s → IO (Maybe (a, s))
runMaybeState m s = do
    (v, st) ← flip runStateT s $ runMaybeT m
    case v of
        Just x → return $ Just (x, st)
        Nothing → return $ Nothing

type Route a = MaybeState (Request, [Header]) a

data Header = Header B.ByteString B.ByteString | BadHeader
    deriving (Show, Eq)

data Request = Request {
        reqMethod ∷ B.ByteString
      , reqPath ∷ [B.ByteString]
      , reqArgs ∷ [(B.ByteString, B.ByteString)]
      , reqHeaders ∷ [Header]
    } | BadRequest
    deriving Show

data Response = Response {-# UNPACK #-} !Int {-# UNPACK #-} !B.ByteString
              | ResponseL {-# UNPACK #-} !Int {-# UNPACK #-} !BL.ByteString
    deriving Show

-- Response functions --

-- | Returns HTTP 200 response
--
-- > serve 80 [emptyPath >> contentType "text/html" >> okay "Some response"]
okay ∷ B.ByteString → Route Response
okay = return ∘ Response 200
{-# INLINE okay #-}

-- | Returns HTTP 200 response with a lazy 'BL.ByteString'
okayL ∷ BL.ByteString → Route Response
okayL = return ∘ ResponseL 200
{-# INLINE okayL #-}

-- | Returns HTTP 404 response
--
-- > serve 80 [
-- >    emptyPath >> contentType "text/html" >> okay "Some response",
-- >    contentType "text/html" >> notFound "<h3>404 Not Found</h3>"
-- > ]
notFound ∷ B.ByteString → Route Response
notFound = return ∘ Response 404
{-# INLINE notFound #-}

-- | Returns HTTP 404 response with a lazy 'BL.ByteString'
notFoundL ∷ BL.ByteString → Route Response
notFoundL = return ∘ ResponseL 404
{-# INLINE notFoundL #-}

-- | Returns HTTP 404 response with a lazy 'BL.ByteString'
serveFile ∷ BL.ByteString → Route Response
serveFile = return ∘ ResponseL 404

-- Route functions --

-- | Sets response header
header ∷ B.ByteString → B.ByteString → Route ()
header n v = lift $ modify (\(r, hs) → (r, Header n v : filter (\(Header k _) → k ≠ n) hs))
{-# INLINE header #-}

-- | Sets Content-Type response header
contentType ∷ B.ByteString → Route ()
contentType = header $ B.pack "Content-Type"
{-# INLINE contentType #-}

-- | Returns path stack
pathList ∷ Route [B.ByteString]
pathList = lift get >>= return ∘ reqPath ∘ fst

-- | Returns path stack
fullPath ∷ B.ByteString → Route ()
fullPath p = pathList >>= guard ∘ (≡) p ∘ B.concat ∘ concatMap (\x → [B.pack "/", x])

-- | Removes the top element from the path stack and checks that it matches the input
path ∷ B.ByteString → Route ()
path s = popPath >>= guard ∘ (≡) s
{-# INLINE path #-}

-- | Pops the top element off the path stack
popPath ∷ Route B.ByteString
popPath = do
    (r, rhs) ← lift get
    guard ∘ not ∘ null $ reqPath r
    lift $ put (r{reqPath=tail $ reqPath r}, rhs)
    return ∘ head $ reqPath r 

-- | Checks that the path stack is empty
emptyPath ∷ Route ()
emptyPath = lift get >>= guard ∘ null ∘ reqPath ∘ fst
{-# INLINE emptyPath #-}

-- | Returns the request headers
headerList ∷ Route [Header]
headerList = reqHeaders ∘ fst <$> lift get
{-# INLINE headerList #-}

-- | Return the value of the request header that matches the given name or 'Nothing' if the header isn't present
getHeader ∷ B.ByteString → Route (Maybe B.ByteString)
getHeader n = do
    hs ← headerList
    case filter (\(Header k _) → k ≡ n) hs of
        (Header _ v:xs) → return $ Just v
        _ → return Nothing

-- | Checks that the value of the Host header matches the given hostname
host ∷ B.ByteString → Route ()
host h = (getHeader $ B.pack "Host") >>= guard ∘ (≡) (Just h)

runRoutes ∷ [Route Response] → (Request, [Header]) → IO (Maybe (Response, [Header]))
runRoutes rs s = do
    v ← mapM (flip runMaybeState s) rs >>= return ∘ msum
    case v of
        Just (r, (_, hs)) → return $ Just (r, hs)
        Nothing → return Nothing