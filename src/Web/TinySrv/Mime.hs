{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Web.TinySrv.Mime where

import Data.ByteString (ByteString)

getMime ∷ ByteString → ByteString
getMime ".html" = "text/html"
getMime ".css" = "text/css"
getMime ".js" = "application/javascript"
getMime ".xhtml" = "application/xhtml+xml"

getMime ".xml" = "application/xml"
getMime ".yaml" = "text/yaml"

getMime ".png" = "image/png"
getMime ".gif" = "image/gif"
getMime ".jpeg" = "image/jpeg"
getMime ".jpg" = "image/jpeg"
getMime ".bmp" = "image/bmp"
getMime ".webp" = "image/webp"
getMime ".svg" = "image/svg+xml"
getMime ".ico" = "image/x-icon"

getMime ".avi" = "video/x-msvideo"
getMime ".flv" = "video/x-flv"
getMime ".mpeg" = "video/mpeg"
getMime ".ogv" = "video/ogg"
getMime ".webm" = "video/webm"
getMime ".mp4" = "video/mp4"
getMime ".h261" = "video/h261"
getMime ".h263" = "video/h263"
getMime ".h264" = "video/h264"
getMime ".jpgv" = "video/jpeg"

getMime ".oga" = "audio/ogg"
getMime ".mp3" = "audio/mpeg3"

getMime ".bz" = "application/x-bzip"
getMime ".bz2" = "application/x-bzip2"
getMime ".tar.bz" = "application/x-bzip"
getMime ".tar.bz2" = "application/x-bzip2"
getMime ".tar.gz" = "application/gzip"
getMime ".gz" = "application/gzip"
getMime ".zip" = "application/zip"
getMime ".7z" = "application/x-7z-compressed"

getMime ".rss" = "application/rss"
getMime ".json" = "application/json"
getMime ".torrent" = "application/x-bittorrent"
getMime ".txt" = "text/plain"

getMime ".ttf" = "application/x-font-ttf"
getMime ".otf" = "application/x-font-otf"
getMime ".woff" = "application/x-font-woff"

getMime ".exe" = "application/x-msdownload"
getMime _ = "application/octet-stream"