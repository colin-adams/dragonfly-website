module Dragonfly.Authorization.Group (
                                      Group (..),
                                      newGroup,
                                      hasCapability
                                     ) where

import Database.CapabilitiesTable
import Database.HaskellDB
import Database.HaskellDB.Database

-- | Groups of users with the same authorizations.
data Group = Group {name :: String, capabilities :: [String] } deriving (Read, Show, Eq)

-- | Read group definition from database
newGroup :: Database -> String -> IO (String, Group)
newGroup db nm = do
  let q = do
        t <- table capabilitiesTable
        return t
  rs <- query db q
  let cps = map (\rec -> rec!capability) rs
  return $ (nm, Group nm cps)

-- | Does the group have the capability?
hasCapability :: String -> Group -> Bool
hasCapability capability group = 
    let cs = capabilities group
    in capability `elem` cs
