module Dragonfly.Authorization.Password (
                 encryptPassword
                ) where

import Codec.Utils (listToOctets, listFromOctets)

import Data.Char (ord, chr)
import Data.Digest.SHA512

-- | One-way encyryption of argument
encryptPassword :: String -> String
encryptPassword p = map chr $ map fromIntegral $ hash . listToOctets $ map ord p