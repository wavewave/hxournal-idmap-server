{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Database.HXournal.IDMap.Server.Parse.Multipart
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Database.HXournal.IDMap.Server.Parse.Multipart where

import Control.Applicative (many)
import Data.Text hiding (takeWhile)
-- import Data.Attoparsec
import Data.Attoparsec.Text
import Prelude hiding (takeWhile)

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy as L

-- | 

multipartHeader :: Parser Text
multipartHeader = do 
  string "multipart/form-data; boundary="
  takeWhile (not . flip elem " \n\r")

-- | 

separateUsingBdry :: L.ByteString -> [L.ByteString]
separateUsingBdry bstr = LC.lines bstr

