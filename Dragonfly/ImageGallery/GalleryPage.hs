-- | Display a page to show one gallery, its immediate child pictures, and sub-galleries.
module Dragonfly.ImageGallery.GalleryPage (
                                           displayGalleryTree
                                          ) where

import Control.Monad.Reader
import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy.UTF8 as LU

import Numeric

import System.Time

import Database.HaskellDB.Database as DB

import Happstack.Server.SimpleHTTP

import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((<<), (+++)) 

import Dragonfly.ApplicationState
import Dragonfly.ImageGallery.Database
import Dragonfly.URISpace

-- | Display a gallery tree and immediate child pictures
displayGalleryTree :: Maybe Input -> Maybe Input -> Maybe Cookie -> MyServerPartT Response
displayGalleryTree galleryParam pageParam sc = do
  ApplicationState db sessions <- lift ask
  sess <- liftIO $ readMVar sessions
  (header, galleries, pictures) <- 
    case galleryParam of
      Nothing -> liftIO $ 
                do
                  gs <-  topLevelGalleries db
                  return ("Image galleries", gs, [])
      Just g -> liftIO $
               do
                 let gName = LU.toString $ inputValue g
                 gs <- childGalleries db gName
                 pics <- images db gName
                 return (gName, gs, pics)
  authorizedGalleries <- liftIO $ filterM (isGalleryAuthorized sc sess) galleries
  authorizedGalleryHeadlines <- liftIO $ mapM (readHeadline db) authorizedGalleries
  galDiv <- liftIO $ galleriesDiv header authorizedGalleryHeadlines db pictures pageParam
  ok $ toResponse $ galleryHeader +++ (X.body << galDiv)

-- | Html head for galleries
galleryHeader :: X.Html
galleryHeader = X.header <<
                (X.style X.! [X.thetype "text/css"]
                  << "@import url(/styles/defaults.css); @import url(/styles/system.css); @import url(/styles/image_gallery.css); @import url(/styles/style.css);")

-- | Display headline list of galleries      
galleriesDiv :: String -> [GalleryHeadline] -> Database -> [Integer] -> Maybe Input -> IO X.Html
galleriesDiv header galleries db pics pageParam = do
    let gallsDiv = X.ulist X.! [X.theclass "galleries"] << map displayGallery galleries
    picsDiv <- if null pics
              then return X.noHtml
              else picturesDiv header db pics pageParam
    return $ X.h1 << header
       +++ gallsDiv
       +++ picsDiv

-- | HTML div showing first 10 thumbnails in gallery
picturesDiv :: String -> Database -> [Integer] -> Maybe Input -> IO X.Html
picturesDiv header db pics pageParam = do
  let pg = decodedPageNumber pageParam
      maximumThumbnails = 10
      dropCount = pg * maximumThumbnails
      totalThumbnails = length pics
      indices = take maximumThumbnails (drop dropCount pics)
  picsInfo <- pictureInfo db indices
  return $ (X.thediv << X.ulist X.! [X.theclass "images"] << X.concatHtml (map pictureInfoItem picsInfo))
    +++ if totalThumbnails <= maximumThumbnails
        then X.noHtml
        else pagerDiv header pg maximumThumbnails totalThumbnails


-- | Html for gallery pager (links)
pagerDiv :: String -> Int -> Int -> Int -> X.Html
pagerDiv header currentPage thumbnailsPerPage thumbnailCount = X.thediv X.! [X.theclass "pager"] << contents
    where contents = goToFirst +++ goToPrevious +++ mainPager +++ goToNext +++ goToLast
          newRequest = imageGalleryURL ++ "?" ++ galleryParameter ++ "=" ++ header
          criticalPageNumber = 5                              
          lastPage = (thumbnailCount - 1) `div` thumbnailsPerPage
          goToFirst = if currentPage == 0
                      then X.noHtml
                      else X.anchor X.! [X.href newRequest, X.title "Go to first page", 
                                         X.theclass "pager-first active"] << "« first" 
          goToPrevious = if currentPage == 0
                      then X.noHtml
                      else X.anchor X.! [X.href $ newRequest ++ "&page=" ++ show (currentPage - 1), 
                                         X.title "Go to previous page", X.theclass "pager-previous active"] << "‹ previous" 
          goToLast = if currentPage == lastPage
                      then X.noHtml
                      else X.anchor X.! [X.href $ newRequest ++ "&page=" ++ show lastPage, 
                                         X.title "Go to last page", X.theclass "pager-last active"] << "last »" 
          goToNext = if currentPage == lastPage
                      then X.noHtml
                      else X.anchor X.! [X.href $ newRequest ++ "&page=" ++ show (currentPage + 1), 
                                         X.title "Go to next page", X.theclass "pager-next active"] << "next ›"
          ellipsis = if currentPage < criticalPageNumber
                     then X.noHtml
                     else X.thespan X.! [X.theclass "pager-ellipsis"] << "…"
          mainPager = if thumbnailCount > thumbnailsPerPage
                      then let firstPageNum = if currentPage < criticalPageNumber then 1 else currentPage - 3
                           in X.thespan X.! [X.theclass "pager-list"] << ellipsis +++ 
                              map (pagerSpan currentPage newRequest) [firstPageNum .. (lastPage + 1)]
                      else X.noHtml
                        
pagerSpan :: Int     -- ^ 0-based number of current page
            -> String -- ^ URI stem for target
            -> Int    -- ^ 1-based page number to be display
            -> X.Html
pagerSpan currentPage newRequest targetPage =
  if currentPage == targetPage - 1
  then X.strong X.! [X.theclass "pager-current"] << show targetPage
  else X.anchor X.! [X.theclass "pager-previous active", X.title $ "go to page " ++ show targetPage, 
                 X.href $ newRequest  ++ "&page=" ++ show (targetPage - 1)] << show targetPage

pictureInfoItem :: PictureInfo -> X.Html
pictureInfoItem (thumbnail, preview, caption, uploadTime, user) =
    X.li X.! [X.thestyle "height : 220px; width : 130px;"] << itemContents
      where itemContents = (X.anchor X.! [X.href $ X.stringToHtmlString previewRef] << X.image X.! [X.src thumbnail]) +++ itemTrailer
            itemTrailer = X.h3 << X.anchor X.! [X.href $ X.stringToHtmlString previewRef] << caption +++
                          X.thediv X.! [X.theclass "author"] << ("Posted by: " ++ user) +++
                          X.thediv X.! [X.theclass "date"] << calendarTimeToString uploadTime
            previewRef = imageGalleryURL ++ "?" ++ previewParameter ++ "=" ++ preview                        


-- | Display one gallery headline
displayGallery :: GalleryHeadline -> X.Html
displayGallery (GalleryHeadline name count picture) =
 X.li X.! [X.theclass "clear-block"] << itemContents
   where itemContents = (X.anchor X.! [X.href $ X.stringToHtmlString $ imageGalleryURL ++ "?" ++ galleryParameter ++ "=" ++ name] << name) +++ itemTail
         itemTail = let toBe = case count of
                          1 -> "is "
                          _ -> "are "
                        pictureNoun = case count of
                          1 -> " picture"
                          _ -> " pictures"
                        pictureCountPhrase = "There " ++ toBe ++ show count ++ pictureNoun ++ " in this gallery."
                    in case count of
                      0 -> X.p << "There are no pictures in this gallery"
                      _ -> case picture of
                        Just (image, preview, original, date) -> 
                          (X.anchor X.! [X.href $ X.stringToHtmlString previewRef] << 
                           (X.image X.! [X.src image])) +++
                          X.p X.! [X.theclass "count"] << pictureCountPhrase +++ 
                          X.p X.! [X.theclass "last"] << updatedPhrase
                            where previewRef = imageGalleryURL ++ "?" ++ previewParameter ++
                                         "=" ++ preview
                                  updatedPhrase = "Last updated: " ++ 
                                            calendarTimeToString date ++ " UTC"

-- | Page number from URI parameter if present and legal. Otherwise 0.
decodedPageNumber :: Maybe Input -> Int
decodedPageNumber pageParam =
  case pageParam of
    Just p -> 
      let pageString = LU.toString $ inputValue p
          pageNum = readDec pageString
      in case pageNum of
        [] -> 0
        (pg, rem):[] -> if null rem then pg else 0
        _ -> 0
    Nothing -> 0
