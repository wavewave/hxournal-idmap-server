{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Database.HXournal.IDMap.Server.Type
import Database.HXournal.IDMap.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

import System.Console.CmdArgs
import Database.HXournal.IDMap.Server.ProgType
import Database.HXournal.IDMap.Type
import Data.Aeson as A 


import Control.Applicative
import Data.UUID
import Data.Time.Clock
import qualified Data.ByteString.Lazy as LS

data HXournalIDMapInfoOld = HXournalIDMapInfoOld { 
  hxournal_idmap_uuid :: UUID, 
  hxournal_idmap_name :: String, 
  hxournal_idmap_creationtime :: UTCTime,
  hxournal_idmap_numofpages :: Int
} deriving (Show,Typeable,Data)



instance FromJSON HXournalIDMapInfoOld where
  parseJSON (Object v) = HXournalIDMapInfoOld 
                         <$> v .: "uuid"
                         <*> v .: "name" 
                         <*> v .: "creationtime"
                         <*> v .: "numofpages"


convertOldToNew :: HXournalIDMapInfoOld -> HXournalIDMapInfo 
convertOldToNew (HXournalIDMapInfoOld uuid name ctime npages) =
  HXournalIDMapInfo uuid name ctime 0 npages 


main :: IO ()
main = do 
  putStrLn "hxournal-idmap-server"
  param <- cmdArgs mode
  commandLineProcess param 


commandLineProcess :: HXournalIDMapServerCommand -> IO ()
commandLineProcess Server = do 
  acid <- openLocalState M.empty 
  warpDebug 7801 (HXournalIDMapServer acid)
commandLineProcess (DumpOut filepath) = do 
  putStrLn "dumpOut"
  acid <- openLocalState M.empty 
  lst <- query acid QueryAll 
  putStrLn $ show $ length lst 
  let encoded = A.encode lst
  LS.writeFile filepath  encoded


commandLineProcess (DumpIn filepath)  = do 
  putStrLn "dumpIn"
  encoded <- LS.readFile filepath
  putStrLn "---------------"
  let decoded = (A.decode $ encoded :: Maybe [HXournalIDMapInfoOld]) 
  case decoded of 
    Nothing -> return () 
    Just lst -> do 
      let newlst = map convertOldToNew lst
      acid <- openLocalState M.empty 
      mapM_ (\x -> Data.Acid.update acid (AddHXournalIDMap x)) newlst
      --  putStrLn $ show $ take 3 newlst 
