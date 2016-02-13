{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module TinySrv.Monad (
      Route
    , Request(..)
    , Header(..)
    , Response(..)
    , okay
    , okayL
    , notFound
    , notFoundL
    , header
    , contentType
    , pathList
    , pathString
    , partialPath
    , fullPath
    , path
    , popPath
    , emptyPath
    , headerList
    , getHeader
    , host
    , serveFile
    , serveDirectory
    , runRoutes
) where

import Prelude.Unicode

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad (msum)

import qualified Data.ByteString.Char8 as B (ByteString, pack, unpack, singleton, empty, split, concat)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, readFile)
import Data.List (null, dropWhileEnd)

import System.Directory

type MaybeState s a = MaybeT (StateT s IO) a

runMaybeState ∷ MaybeState s a → s → IO (Maybe (a, s))
runMaybeState m s = do
    (v, st) ← flip runStateT s $ runMaybeT m
    case v of
        Just x → return $ Just (x, st)
        Nothing → return Nothing

type Route a = MaybeState (Request, [Header]) a

data Header = Header {-# UNPACK #-} !B.ByteString {-# UNPACK #-} !B.ByteString | BadHeader
    deriving (Show, Eq)

data Request = Request {
        reqMethod ∷ B.ByteString
      , reqPath ∷ [B.ByteString]
      , reqArgs ∷ [(B.ByteString, B.ByteString)]
      , reqHeaders ∷ [Header]
    } | BadRequest
    deriving Show

data Response = Response {-# UNPACK #-} !Int {-# UNPACK #-} !B.ByteString
              | ResponseL {-# UNPACK #-} !Int BL.ByteString
    deriving Show

-- Response functions --

-- | Returns HTTP 200 response
--
-- > serve 80 [emptyPath >> contentType "text/html" >> okay "Some response"]
okay ∷ B.ByteString -- ^ Response body
     → Route Response
okay = return ∘ Response 200
{-# INLINE okay #-}

-- | Returns HTTP 200 response with a lazy 'BL.ByteString'
okayL ∷ BL.ByteString -- ^ Response body
      → Route Response
okayL = return ∘ ResponseL 200
{-# INLINE okayL #-}

-- | Returns HTTP 404 response
--
-- > serve 80 [ emptyPath >> contentType "text/html" >> okay "Some response"
-- >          , contentType "text/html" >> notFound "<h3>404 Not Found</h3>"
-- >          ]
notFound ∷ B.ByteString -- ^ Response body
         → Route Response
notFound = return ∘ Response 404
{-# INLINE notFound #-}

-- | Returns HTTP 404 response with a lazy 'BL.ByteString'
notFoundL ∷ BL.ByteString -- ^ Response body
          → Route Response
notFoundL = return ∘ ResponseL 404
{-# INLINE notFoundL #-}

-- Route functions --

-- | Returns the request headers
headerList ∷ Route [Header]
headerList = reqHeaders ∘ fst <$> lift get
{-# INLINE headerList #-}

-- | Return the value of the request header if present, or 'Nothing' if not
getHeader ∷ B.ByteString -- ^ Header name
          → Route (Maybe B.ByteString) -- ^ Header value
getHeader n = do
    hs ← headerList
    case filter (\(Header k _) → k ≡ n) hs of
        (Header _ v:xs) → return $ Just v
        _ → return Nothing

-- | Set response header
header ∷ B.ByteString -- ^ Header name
       → B.ByteString -- ^ Header value
       → Route ()
header n v = lift $ modify (\(r, hs) → (r, Header n v : filter (\(Header k _) → k ≠ n) hs))
{-# INLINE header #-}

-- | Set Content-Type header
--
-- > serve 80 [contentType "application/json" >> okay "{\"a\":1}"]
contentType ∷ B.ByteString → Route ()
contentType = header "Content-Type"
{-# INLINE contentType #-}

-- | Returns path stack
pathList ∷ Route [B.ByteString]
pathList = lift get >>= return ∘ reqPath ∘ fst

-- | Returns path stack as a string joined with \'/\'
pathString ∷ Route B.ByteString
pathString = lift get >>= return ∘ B.concat ∘ concatMap (\x → [B.singleton '/', x]) ∘ reqPath ∘ fst
    where
        addSlashToEmpty x | x ≡ B.empty = B.singleton '/'
                          | otherwise   = x

-- | Checks that the top elements of the stack path match the input, and removes them
--
-- > serve 80 [partialPath "/abc/123/" >> pathString >>= okay]
partialPath ∷ B.ByteString → Route ()
partialPath p = foldl (>>) (return ()) $ map path (filter (≠ B.empty) $ B.split '/' p)

-- | Check that the request path matches the input
fullPath ∷ B.ByteString → Route ()
fullPath p = partialPath p >> emptyPath
{-# INLINE fullPath #-}

-- | Pop the top element from the path stack and check that it matches the input
--
-- > serve 80 [ emptyPath >> okay "This is /"
-- >          , path "abc" >> emptyPath >> okay "This is /abc"
-- >          , path "abc" >> path "123" >> emptyPath >> okay "This is /abc/123"
-- >          ]
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

-- | Hostname guard
host ∷ B.ByteString -- ^ Hostname
     → Route ()
host h = getHeader "Host" >>= guard ∘ (≡) (Just h)

-- | Serve a file
serveFile ∷ FilePath -- ^ Path to file
          → B.ByteString -- ^ Routing path
          → Route Response
serveFile f p = fullPath p >> liftIO (BL.readFile f) >>= okayL

-- | Serve a directory
serveDirectory ∷ Bool -- ^ Allow file index
               → FilePath -- ^ Path to the directory
               → B.ByteString -- ^ Routing path
               → Route Response
serveDirectory l d p = do
    partialPath p
    urlP ← pathString
    let p' = dropWhileEnd (≡ '/') d ++ B.unpack urlP
    fe ← liftIO $ doesFileExist p'
    de ← liftIO $ doesDirectoryExist p'
    guard (fe ∨ de ∧ l)
    if fe
        then liftIO (BL.readFile p') >>= okayL
        else do
            cs ← filter (≠ ".") <$> liftIO (getDirectoryContents p')
            okay $ B.concat ["<html><head><title>Index of ", urlP, "</title></head><body><h2>Index of "
                            , urlP, "</h2><hr>", B.concat ∘ map B.pack $ (\x → ["<a href=\"", x, "\">", x, "</a><br>"]) =<< cs
                            , "</body></html>"]

runRoutes ∷ [Route Response] → (Request, [Header]) → IO (Maybe (Response, [Header]))
runRoutes rs s = do
    v ← mapM (`runMaybeState` s) rs >>= return ∘ msum
    case v of
        Just (r, (_, hs)) → return $ Just (r, hs)
        Nothing → return Nothing