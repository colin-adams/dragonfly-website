module Dragonfly.Authorization.Password (
                                         stringToSalt,
                                         saltToString,
                                         checkSalt,
                                         buildSaltAndHash
                                        ) where

import Codec.Utils
import Control.Monad

import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Digest.SHA512

import Numeric

import System.Random

-- | Convert from Unicode Strings (stored in database) to salt
stringToSalt :: String -> SaltedHash
stringToSalt = SaltedHash . B.unpack . strToBytes

-- | Convert from SaltedHash to Unicode Strings (to be stored in database)
saltToString :: SaltedHash -> String
saltToString (SaltedHash h) =  bytesToStr . B.pack $ h

-- | Hash a password
buildSaltAndHash :: String -> IO SaltedHash
buildSaltAndHash str = do
  salt <- randomSalt
  let salt' = strToOctets salt
  let str' = strToOctets str
  let h = slowHash (salt' ++ str')
  return $ SaltedHash $ salt' ++ h

-- | Check salt is valid for password
checkSalt :: String -> SaltedHash -> Bool
checkSalt str (SaltedHash h) = h == salt ++ slowHash (salt ++ strToOctets str)
  where salt = take saltLength h


-- | Type for encrypted passwords
newtype SaltedHash = SaltedHash [Octet] deriving (Read, Show, Ord, Eq)

-- | Length for salted hash 
saltLength :: Int
saltLength = 16

-- | Convert Unicode strings to bytes
strToOctets :: String -> [Octet]
strToOctets = listToOctets . map c2w

-- | Hash bytes slowly (to make brute-force attacks harder)
slowHash :: [Octet] -> [Octet]
slowHash a = iterate hash a !! 512

-- | Random salt as a string
randomSalt :: IO String
randomSalt = liftM concat $ sequence $ take saltLength $ repeat $ randomRIO (0::Int,15) >>= return . flip showHex ""

-- | Convert from Unicode strings to lazy ByteStrings
strToBytes :: String -> B.ByteString
strToBytes = read

-- | Convert from lazy ByteStrings to Unicode strings
bytesToStr :: B.ByteString -> String
bytesToStr = show

