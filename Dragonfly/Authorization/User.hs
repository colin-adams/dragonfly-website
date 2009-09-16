{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dragonfly.Authorization.User where

import Dragonfly.Authorization.Group

import System.Random

newtype SessionKey = SessionKey Integer deriving (Read, Show, Ord, Eq, Num, Random)

-- | Groups indexed by name
type Groups = [(String, Group)]

-- | Logged-in users
data User = User {name :: String, sessionKey :: SessionKey, groups :: Groups} deriving  (Read, Show, Eq)

-- | Is user authorized to a specific capability?
authorizedTo :: User -> String -> Bool
authorizedTo user capability =
    let gs = map snd (groups user)
    in any (hasCapability capability) gs
    
