{-# LANGUAGE UnicodeSyntax #-}
module TinySrv.Monad where

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

okay ∷ ByteString → Response
okay = Response 200

notFound ∷ ByteString → Response
notFound = Response 404

-- Route functions --

--Set response header
header ∷ ByteString → ByteString → Route ()
header n v = do
    (r, hs) ← lift get
    lift $ put (r, Header n v:hs)

pathList ∷ Route [ByteString]
pathList = lift get >>= return ∘ reqPath ∘ fst

path ∷ ByteString → Route ()
path s = do
    (r, rhs) ← lift get
    guard ∘ not ∘ null $ reqPath r
    guard $ head (reqPath r) ≡ s
    lift $ put (r{reqPath=tail $ reqPath r}, rhs)

emptyPath ∷ Route ()
emptyPath = lift get >>= guard ∘ null ∘ reqPath ∘ fst

headerList ∷ Route [Header]
headerList = reqHeaders ∘ fst <$> lift get

getHeader ∷ ByteString → Route (Maybe ByteString)
getHeader b = do
    hs ← headerList
    case filter (\(Header k _) → k ≡ b) hs of
        (Header _ v:xs) → return $ Just v
        _ → return Nothing

host ∷ ByteString → Route ()
host b = do
    h ← getHeader $ pack "Host"
    guard (h ≡ Just b)

runRoutes ∷ [Route Response] → (Request, [Header]) → Maybe (Response, [Header])
runRoutes rs s = (\(r, (_, hs)) → (r, hs)) <$> (msum $ map (flip runMaybeState s) rs)