module Web.Passpb.Crypt where

import Crypto.Hash.MD5 (hash)

import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base64 (encode)

scramble :: String -> String
scramble key = B8.unpack . encode . hash $ B8.pack key
