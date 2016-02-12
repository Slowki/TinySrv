{-# LANGUAGE UnicodeSyntax #-}
module TinySrv.Monad (
      Route
    , Request(..)
    , Header(..)
    , Response(..)
    , okay
    , notFound
    , header
    , contentType
    , pathList
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
import Data.ByteString.Char8 (ByteString, pack)
import Control.Monad (msum)
import Data.List (null)

type MaybeState s a = MaybeT (State s) a

runMaybeState ∷ MaybeState s a → s → Maybe (a, s)
runMaybeState m s =
    let (v, st) = flip runState s $ runMaybeT m in
    case v of
        Just x → Just (x, st)
        Nothing → Nothing

type Route a = MaybeState (Request, [Header]) a

data Header = Header ByteString ByteString | BadHeader
    deriving (Show, Eq)

data Request = Request {
        reqMethod ∷ ByteString
      , reqPath ∷ [ByteString]
      , reqArgs ∷ [(ByteString, ByteString)]
      , reqHeaders ∷ [Header]
    } | BadRequest
    deriving Show

data Response = Response Int ByteString
    deriving Show

-- Response functions --

-- | Returns HTTP 200 response
--
-- > serve 80 [emptyPath >> contentType "text/html" >> okay "Some response"]
okay ∷ ByteString → Route Response
okay = return ∘ Response 200

-- | Returns HTTP 404 response
--
-- > serve 80 [
-- >    emptyPath >> contentType "text/html" >> okay "Some response",
-- >    contentType "text/html" >> notFound "<h3>404 Not Found</h3>"
-- > ]
notFound ∷ ByteString → Route Response
notFound = return ∘ Response 404

-- Route functions --

-- | Sets response header
header ∷ ByteString → ByteString → Route ()
header n v = do
    (r, hs) ← lift get
    lift $ put (r, Header n v : filter (\(Header k _) → k ≠ n) hs)

-- | Sets Content-Type response header
contentType ∷ ByteString → Route ()
contentType = header $ pack "Content-Type"

-- | Returns path broken on /
pathList ∷ Route [ByteString]
pathList = lift get >>= return ∘ reqPath ∘ fst

-- | Removes the top element from the path stack and checks that it matches the input
path ∷ ByteString → Route ()
path s = popPath >>= guard ∘ (≡) s

-- | Pops the top element off the path stack
popPath ∷ Route ByteString
popPath = do
    (r, rhs) ← lift get
    guard ∘ not ∘ null $ reqPath r
    lift $ put (r{reqPath=tail $ reqPath r}, rhs)
    return ∘ head $ reqPath r 

-- | Checks that the path stack is empty
emptyPath ∷ Route ()
emptyPath = lift get >>= guard ∘ null ∘ reqPath ∘ fst

-- | Returns the request headers
headerList ∷ Route [Header]
headerList = reqHeaders ∘ fst <$> lift get

-- | Return the value of the request header that matches the given name or 'Nothing' if the header isn't present
getHeader ∷ ByteString → Route (Maybe ByteString)
getHeader n = do
    hs ← headerList
    case filter (\(Header k _) → k ≡ n) hs of
        (Header _ v:xs) → return $ Just v
        _ → return Nothing

-- | Checks that the value of the Host header matches the given hostname
host ∷ ByteString → Route ()
host h = do
    h' ← getHeader $ pack "Host"
    guard (h' ≡ Just h)

runRoutes ∷ [Route Response] → (Request, [Header]) → Maybe (Response, [Header])
runRoutes rs s = (\(r, (_, hs)) → (r, hs)) <$> (msum $ map (flip runMaybeState s) rs)