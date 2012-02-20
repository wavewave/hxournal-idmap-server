{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.HXournal.IDMap.Client.Job
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Database.HXournal.IDMap.Server.Job.FileWork where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as SC
import qualified Data.Text as T 
import Data.UUID
import Data.UUID.V5
import Database.HXournal.IDMap.Type
import Database.HXournal.Store.Job
import System.Directory
import System.FilePath
import Data.Time.Clock
import Yesod.Request
-- import System.Environment


-- | 
 
nextUUID :: IO UUID
nextUUID = do 
  t <- getCurrentTime 
  return . generateNamed namespaceURL . B.unpack . SC.pack $ show t 

-- | 

addFileToRepo :: FileInfo -> IO (Maybe HXournalIDMapInfo)
addFileToRepo finfo = do 
    uuid <- nextUUID 
    tmpdir <- getTemporaryDirectory
    let fname = T.unpack . fileName $ finfo 
        fcont = fileContent finfo
        tfname = tmpdir </> fname 
    LB.writeFile tfname fcont
    utctime <- getCurrentTime 
    npages <- startAdd (toString uuid) tfname 
    let info = HXournalIDMapInfo { hxournal_idmap_uuid = uuid 
                                 , hxournal_idmap_name = fname 
                                 , hxournal_idmap_creationtime = utctime
                                 , hxournal_idmap_currentversion = 0
                                 , hxournal_idmap_numofpages = npages 
                                 } 
    
    putStrLn $ show info
    return (Just info)




