-- | Definition of a blog entry
module Dragonfly.Blog.BlogEntry (BlogEntry) where

import Dragonfly.Authorization.User

-- | Fields of an entry in a user's blog
data BlogEntry = BlogEntry {user :: User, title :: String, pandocText :: String}



       
