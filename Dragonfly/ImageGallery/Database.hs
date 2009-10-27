-- | Database routines for image galleries

module Dragonfly.ImageGallery.Database where

import Data.List (nub)
import Data.Tree

import System.Time

import Happstack.Server.SimpleHTTP (Cookie, cookieValue)

import Database.HaskellDB
import Database.HaskellDB.Database
import qualified Database.GalleryTable as GT
import qualified Database.GalleryImageTable as GIT
import qualified Database.ImageTable as IT

import Dragonfly.ApplicationState
import qualified Dragonfly.Authorization.Authorities as Auth
import qualified Dragonfly.Authorization.User as U

-- | Record from database defining a gallery
data Gallery = Gallery {
      name :: String,
      parent :: Maybe String,
      readCapabilityName :: String,
      uploadCapabilityName :: String,
      administrationCapabilityName :: String
    } deriving Show

-- | Gallery name, number of pictures, and if non-zero, latest picture, and upload date
data GalleryHeadline =  GalleryHeadline {gName :: String,   -- ^Name of gallery
                                         count :: Int,      -- ^Number of pictures (including all in sub-galleries)
                                         picture :: Maybe (String, String, String, CalendarTime) -- ^File names and upload time of latest picture 
                                        }

-- | details of a single picture from database
type PictureInfo = (String, -- thumbnail name
                    String, -- preview name
                    String, -- title
                    CalendarTime, -- upload time
                    String -- user name
                   )

-- | Get list of all top-level galleries from database
topLevelGalleries :: Database -> IO [Gallery]
topLevelGalleries db = do
  let q = do
        t <- table GT.galleryTable
        restrict (isNull $ t ! GT.parentGalleryName)
        return t
  rs <- query db q
  return $ map newGallery rs

-- | All galleries in database
allGalleries :: Database -> IO [Gallery]
allGalleries db = do
  rs <- query db (table GT.galleryTable)
  return $ map newGallery rs

-- | All galleries names to which the user can upload images
authorizedUploadGalleries :: U.User -> Database -> IO [String]
authorizedUploadGalleries user db = do
  rs <- query db (table GT.galleryTable)
  let frs = filter (U.authorizedTo user . (! GT.uploadImageCapabilityName)) rs
  return $ map (! GT.galleryName) frs

-- | Contruct a Gallery from its database record
-- Signature commented out as it needs a really long context.
--newGallery :: Record vr -> Gallery
newGallery rec = Gallery (rec ! GT.galleryName) (rec ! GT.parentGalleryName) (rec ! GT.readImageCapabilityName)  (rec ! GT.uploadImageCapabilityName)  (rec ! GT.administerGalleryCapabilityName) 


-- | Get list of immediate child galleries of a named gallery
childGalleries :: Database -> String -> IO [Gallery]
childGalleries db gName = do
  let q = do
        t <- table GT.galleryTable
        restrict (t ! GT.parentGalleryName .==. constJust gName)
        return t
  rs <- query db q
  return $ map newGallery rs

-- | get the (non-recursive) image indexNumbers from a given named gallery
images :: Database -> String -> IO [Integer]
images db gName = do
  let q = do
        t <- table GIT.galleryImageTable
        restrict (t ! GIT.galleryName .==. constant gName)
        project (GIT.indexNumber << t ! GIT.indexNumber)
  rs <- query db q
  return $ map (! GIT.indexNumber) rs

-- | Check authorization of user to view gallery
isGalleryAuthorized :: Maybe Cookie -> Sessions -> Gallery -> IO Bool
isGalleryAuthorized cook sessions gallery =
  case cook of
    Nothing -> return defaultAuth
    Just c -> do
              let key = cookieValue c
                  u = session sessions (read key)
              case u of
                Nothing -> return defaultAuth
                Just user -> return (U.authorizedTo user capability || defaultAuth)
    where capability = readCapabilityName gallery 
          defaultAuth = capability == Auth.readGalleryCapabilityName


-- | Read in sufficient information to form a gallery headline display
readHeadline :: Database -> Gallery -> IO GalleryHeadline
readHeadline db gallery = do
  -- get the list of all (recursive) child gallery names
  gNames <- unfoldTreeM_BF (childGalleryNames db) (name gallery)
  imageNumbers <- mapM (images db) (Data.Tree.flatten gNames)
  let indices = nub (concat imageNumbers)
  recentUpload <- mostRecentImage db indices
  return $ GalleryHeadline (name gallery) (length indices) recentUpload

-- | Most recent thumbnail-image from list
mostRecentImage :: Database -> [Integer] -> IO (Maybe (String, String, String, CalendarTime))
mostRecentImage db indices =
  if null indices then return Nothing else
      do let q 
                 = do t <- table IT.imageTable
                      restrict (t ! IT.indexNumber `_in` map constant indices)
                      order [desc t IT.uploadTime]
                      top 1
                      return t
         rs <- query db q
         return $ Just (head rs ! IT.thumbnail, head rs ! IT.preview, 
                             head rs ! IT.original, head rs ! IT.uploadTime)


-- | Get list of names of all immediate child galleries of a named gallery
childGalleryNames :: Database -> String -> IO (String, [String])
childGalleryNames db gName = do
  let q = do
        t <- table GT.galleryTable
        restrict (t ! GT.parentGalleryName .==. constJust gName)
        project (GT.galleryName <<  t ! GT.galleryName)
  rs <- query db q
  return (gName, map (! GT.galleryName) rs)


-- | Information about all pictures with provided indices
pictureInfo :: Database -> [Integer] -> IO [PictureInfo]
pictureInfo db indices = do
  let q = do
        t <- table IT.imageTable
        restrict (t ! IT.indexNumber `_in` map constant indices)
        return t
  rs <- query db q
  return $ map newPictureInfo rs 

-- | Contruct a PictureInfo from its database record
-- Signature commented out as it needs a really long context.
--newPictureInfo :: Record vr -> PictureInfo
newPictureInfo rec = (rec ! IT.thumbnail, rec ! IT.preview, rec ! IT.caption,
                      rec ! IT.uploadTime, rec ! IT.userName)

-- Query database for all details about a picture, given it's preview name.
pictureDetailsFromPreview :: String -> Database -> IO (Maybe (String, String, String, String, CalendarTime))
pictureDetailsFromPreview previewName db = do
  let q = do 
        t <- table IT.imageTable
        restrict (t ! IT.preview .==. constant previewName)
        project (IT.thumbnail <<  t ! IT.thumbnail # 
                    IT.original <<  t ! IT.original # 
                    IT.caption <<  t ! IT.caption # 
                    IT.body <<  t ! IT.body # 
                    IT.uploadTime <<  t ! IT.uploadTime
                   )
  rs <- query db q
  if null rs
     then return Nothing
     else return $ Just (head rs ! IT.thumbnail, head rs ! IT.original, head rs ! IT.caption,  
                         head rs ! IT.body,  head rs ! IT.uploadTime)  


-- | Save image information to database
saveImageInfo :: U.User -> Database -> String -> String -> [String] -> String -> (String, String, String) -> IO ()
saveImageInfo user db caption description galleries imageType (thumbnailName, previewName, originalName) = do
  let q = do
        t <- table IT.imageTable
        order [desc t IT.indexNumber]
        project (IT.indexNumber << t ! IT.indexNumber)
  rs <- query db q
  let nextIndex = if null rs
                  then 1
                  else 1 + (head rs ! IT.indexNumber)
  ct <- getClockTime 
  let utc = toUTCTime ct
  insert db IT.imageTable (IT.indexNumber <<- nextIndex  # IT.caption <<- caption # 
                                IT.body <<- description # IT.thumbnail <<- thumbnailName # 
                                IT.preview <<- previewName # IT.original <<- originalName # 
                                IT.uploadTime <<- utc # IT.imageType <<- imageType # IT.userName <<- U.name user)
  mapM_ (\name -> insert db GIT.galleryImageTable (GIT.galleryName <<- name # GIT.indexNumber <<- nextIndex)) galleries
