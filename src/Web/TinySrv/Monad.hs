{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ExistentialQuantification #-}
module Web.TinySrv.Monad (
      Route
    , Request(..)
    , Header(..)
    , HPutable
    , writeStream
    , streamLength
    , Response(..)
    , okay
    , okay'
    , notFound
    , notFound'
    , badRequest
    , badRequest'
    , header
    , contentType
    , method
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

import qualified Data.ByteString.Char8 as B (ByteString, readFile, pack, unpack, singleton, empty, split, concat, hPut, length)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, hPut, length)
import qualified Data.Text as T (Text, length)
import qualified Data.Text.IO as T (hPutStr)
import Data.List (null, dropWhileEnd)

import System.Directory

import System.IO (Handle)

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

class HPutable a where
    writeStream  ∷ Handle → a → IO () -- | Usually just hPut, or a conversion to ByteString then hPut
    streamLength ∷ a → Int            -- | Should return the length of the stream in bytes

instance HPutable B.ByteString where
    writeStream  = B.hPut
    streamLength = B.length

instance HPutable BL.ByteString where
    writeStream  = BL.hPut
    streamLength = fromIntegral ∘ BL.length

instance HPutable T.Text where
    writeStream  = T.hPutStr
    streamLength = T.length

data Response = ∀ a. HPutable a ⇒ Response {-# UNPACK #-} !Int a
-- Response functions --

-- | Returns HTTP 200 response
--
-- > serve 80 [emptyPath >> contentType "text/html" >> okay "Some response"]
okay ∷ HPutable a
     ⇒ a -- ^ Response body
     → Route Response
okay = return ∘ Response 200
{-# INLINE okay #-}

-- | Returns HTTP 200 response with a ByteString for convenience when using OverloadedStrings
okay' ∷ B.ByteString -- ^ Response body
      → Route Response
okay' = return ∘ Response 200
{-# INLINE okay' #-}

-- | Returns HTTP 404 response
--
-- > serve 80 [ emptyPath >> contentType "text/html" >> okay "Some response"
-- >          , contentType "text/html" >> notFound "<h3>404 Not Found</h3>"
-- >          ]
notFound ∷ HPutable a
         ⇒ a -- ^ Response body
         → Route Response
notFound = return ∘ Response 404
{-# INLINE notFound #-}

-- | Returns HTTP 404 response with a ByteString for convenience when using OverloadedStrings
notFound' ∷ B.ByteString -- ^ Response body
          → Route Response
notFound' = return ∘ Response 404
{-# INLINE notFound' #-}

-- | Returns HTTP 400 response
badRequest ∷ HPutable a
         ⇒ a -- ^ Response body
         → Route Response
badRequest = return ∘ Response 400
{-# INLINE badRequest #-}

-- | Returns HTTP 400 response with a ByteString for convenience when using OverloadedStrings
badRequest' ∷ B.ByteString -- ^ Response body
          → Route Response
badRequest' = return ∘ Response 400
{-# INLINE badRequest' #-}


-- Route functions --

-- | Returns the request headers
headerList ∷ Route [Header]
headerList = reqHeaders ∘ fst <$> get
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
header n v = modify (\(r, hs) → (r, Header n v : filter (\(Header k _) → k ≠ n) hs))
{-# INLINE header #-}

-- | Set Content-Type header
--
-- > serve 80 [contentType "application/json" >> okay "{\"a\":1}"]
contentType ∷ B.ByteString → Route ()
contentType = header "Content-Type"
{-# INLINE contentType #-}

-- | Request method guard
method ∷ B.ByteString -- ^ HTTP method
       → Route ()
method m = get >>= guard ∘ (≡) m ∘ reqMethod ∘ fst

-- | Returns path stack
pathList ∷ Route [B.ByteString]
pathList = get >>= return ∘ reqPath ∘ fst

-- | Returns path stack as a string joined with \'/\'
pathString ∷ Route B.ByteString
pathString = get >>= return ∘ B.concat ∘ concatMap (\x → [B.singleton '/', x]) ∘ reqPath ∘ fst
    where
        addSlashToEmpty x | x ≡ B.empty = B.singleton '/'
                          | otherwise   = x

-- | Checks that the top elements of the stack path match the input, and removes them
--
-- > serve 80 [partialPath "/abc/123/" >> pathString >>= okay]
partialPath ∷ B.ByteString → Route ()
partialPath p = foldl (>>) (return ()) $ map path (filter (≠ B.empty) $ B.split '/' p)
{-# INLINE partialPath #-}

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
path s = do
    (r, rhs) ← get
    guard ∘ not ∘ null $ reqPath r
    guard $ head (reqPath r) ≡ s
    put (r{reqPath=tail $ reqPath r}, rhs)
{-# INLINE path #-}

-- | Pop the top element off the path stack
popPath ∷ Route B.ByteString
popPath = do
    (r, rhs) ← get
    guard ∘ not ∘ null $ reqPath r
    put (r{reqPath=tail $ reqPath r}, rhs)
    return ∘ head $ reqPath r 

-- | Checks that the path stack is empty
emptyPath ∷ Route ()
emptyPath = get >>= guard ∘ null ∘ reqPath ∘ fst
{-# INLINE emptyPath #-}

-- | Hostname guard
host ∷ B.ByteString -- ^ Hostname
     → Route ()
host h = getHeader "Host" >>= guard ∘ (≡) (Just h)
{-# INLINE host #-}

--TODO support head requests
-- | Serve a file
serveFile ∷ FilePath -- ^ Path to file
          → B.ByteString -- ^ Routing path
          → Route Response
serveFile f p = fullPath p >> liftIO (B.readFile f) >>= okay
{-# INLINE serveFile #-}

-- | Serve a file or directory
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
        then liftIO (B.readFile p') >>= okay
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
