{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.HXournal.IDMap.Server.Yesod where 

import Yesod hiding (update)
import Network.Wai
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString as S
import Database.HXournal.IDMap.Type
import Data.Acid
import Data.Attoparsec as P
import Data.Aeson as A
import Data.UUID
import Database.HXournal.IDMap.Server.Type

mkYesod "HXournalIDMapServer" [parseRoutes|
/ HomeR GET
/listhxournal-idmap  ListHXournalIDMapR GET
/uploadhxournal-idmap  UploadHXournalIDMapR POST
/hxournal-idmap/#UUID HXournalIDMapR 
|]

instance Yesod HXournalIDMapServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000

{-instance RenderMessage HXournalIDMapServer FormMessage where
  renderMessage _ _ = defaultFormMessage -}


getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [whamlet|
!!!
<html>
  <head> 
    <title> test 
  <body> 
    <h1> hello world 
|]


defhlet :: GGWidget m Handler ()
defhlet = [whamlet| <h1> HTML output not supported |]


getListHXournalIDMapR :: Handler RepHtmlJson
getListHXournalIDMapR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUploadHXournalIDMapR :: Handler RepHtmlJson
postUploadHXournalIDMapR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- return.server_acid =<< getYesod
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result HXournalIDMapInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddHXournalIDMap minfo)
          liftIO $ print (Just r)
          liftIO $ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalIDMapInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalIDMapInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalIDMapInfo))



handleHXournalIDMapR :: UUID -> Handler RepHtmlJson
handleHXournalIDMapR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getHXournalIDMapR name
    "PUT" -> putHXournalIDMapR name
    "DELETE" -> deleteHXournalIDMapR name
    x -> error ("No such action " ++ show x ++ " in handlerHXournalIDMapR")

getHXournalIDMapR :: UUID -> Handler RepHtmlJson
getHXournalIDMapR idee = do 
  liftIO $ putStrLn "getHXournalIDMapR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryHXournalIDMap idee)
  liftIO $ putStrLn $ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


putHXournalIDMapR :: UUID -> Handler RepHtmlJson
putHXournalIDMapR idee = do 
  liftIO $ putStrLn "putHXournalIDMapR called"
  acid <- return.server_acid =<< getYesod
  _wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result HXournalIDMapInfo) of 
        Success minfo -> do 
          if idee == hxournal_idmap_uuid minfo
            then do r <- liftIO $ update acid (UpdateHXournalIDMap minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "hxournal-idmapname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalIDMapInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalIDMapInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalIDMapInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalIDMapInfo))

deleteHXournalIDMapR :: UUID -> Handler RepHtmlJson
deleteHXournalIDMapR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteHXournalIDMap idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))
