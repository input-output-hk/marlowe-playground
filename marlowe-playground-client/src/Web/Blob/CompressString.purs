module Web.Blob.CompressString (compressToURI, decompressFromURI) where

foreign import compressToURI :: String -> String

foreign import decompressFromURI :: String -> String
