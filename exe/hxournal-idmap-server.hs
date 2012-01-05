{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HXournal.IDMap.Server.Type
import Database.HXournal.IDMap.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

main :: IO ()
main = do 
  putStrLn "hxournal-idmap-server"
  acid <- openLocalState M.empty 
  warpDebug 7801 (HXournalIDMapServer acid)