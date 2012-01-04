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

mkYesod "Hxournal-idmapServer" [parseRoutes|
/ HomeR GET
/listhxournal-idmap  ListHxournal-idmapR GET
/uploadhxournal-idmap  UploadHxournal-idmapR POST
/hxournal-idmap/#UUID Hxournal-idmapR 
|]

instance Yesod Hxournal-idmapServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000

{-instance RenderMessage Hxournal-idmapServer FormMessage where
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


getListHxournal-idmapR :: Handler RepHtmlJson
getListHxournal-idmapR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUploadHxournal-idmapR :: Handler RepHtmlJson
postUploadHxournal-idmapR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- return.server_acid =<< getYesod
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result Hxournal-idmapInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddHxournal-idmap minfo)
          liftIO $ print (Just r)
          liftIO $ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe Hxournal-idmapInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe Hxournal-idmapInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe Hxournal-idmapInfo))



handleHxournal-idmapR :: UUID -> Handler RepHtmlJson
handleHxournal-idmapR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getHxournal-idmapR name
    "PUT" -> putHxournal-idmapR name
    "DELETE" -> deleteHxournal-idmapR name
    x -> error ("No such action " ++ show x ++ " in handlerHxournal-idmapR")

getHxournal-idmapR :: UUID -> Handler RepHtmlJson
getHxournal-idmapR idee = do 
  liftIO $ putStrLn "getHxournal-idmapR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryHxournal-idmap idee)
  liftIO $ putStrLn $ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


putHxournal-idmapR :: UUID -> Handler RepHtmlJson
putHxournal-idmapR idee = do 
  liftIO $ putStrLn "putHxournal-idmapR called"
  acid <- return.server_acid =<< getYesod
  _wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result Hxournal-idmapInfo) of 
        Success minfo -> do 
          if idee == hxournal-idmap_uuid minfo
            then do r <- liftIO $ update acid (UpdateHxournal-idmap minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "hxournal-idmapname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe Hxournal-idmapInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe Hxournal-idmapInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe Hxournal-idmapInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe Hxournal-idmapInfo))

deleteHxournal-idmapR :: UUID -> Handler RepHtmlJson
deleteHxournal-idmapR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteHxournal-idmap idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))
