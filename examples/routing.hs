{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Main where

import Web.TinySrv
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Prelude.Unicode

--GET to HOST/somedir/file1 or HOST/somedir/file2
somedir ∷ Route Response
somedir = do
    method "GET"
    path "somedir"
    contentType "text/plain"
    (path "file1" >> emptyPath >> okay "this is file1") <|> (path "file2" >> emptyPath >> okay "this is file2")

--GET to HOST/+/#/#
add ∷ Route Response
add = do
    method "GET"
    path "+"
    n1 ← read ∘ B.unpack <$> popPath ∷ Route Integer
    n2 ← read ∘ B.unpack <$> popPath ∷ Route Integer
    contentType "text/plain"
    okay ∘ B.pack ∘ show $ n1 + n2 --Zero error checking, hooray.

main ∷ IO ()
main = serve 80 [
                  --Serve /srv/http/index.html at HOST/
                  serveFile "/srv/http/index.html" "/"
                  --GET to HOST/somedata.json
                , method "GET" >> path "somedata.json" >> emptyPath >> contentType "application/json" >> okay "{\"data\": 8.7}"
                , somedir
                , add
                  --Catch all, if nothing else succeeded send a 404
                , pathString >>= \x → notFound $ B.concat ["<h3>", x, " not found<h3>"]
                ]