{-# LANGUAGE DeriveDataTypeable #-}

module Database.HXournal.IDMap.Server.ProgType where

import System.Console.CmdArgs hiding (name)

data HXournalIDMapServerCommand = Server 
                                | DumpOut { outfile :: FilePath } 
                                | DumpIn  { infile :: FilePath } 
                           deriving (Show,Typeable,Data)

server :: HXournalIDMapServerCommand
server = Server 

dumpout :: HXournalIDMapServerCommand
dumpout = DumpOut { outfile = "" &= typ "Output File" &= argPos 0 } 

dumpin :: HXournalIDMapServerCommand
dumpin = DumpIn { infile = "" &= typ "Input File" &= argPos 0 } 

mode :: HXournalIDMapServerCommand
mode = modes [ server, dumpout, dumpin ] 
