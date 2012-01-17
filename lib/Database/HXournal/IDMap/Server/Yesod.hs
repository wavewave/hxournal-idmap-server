{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.HXournal.IDMap.Server.Yesod where 

import Control.Applicative

import Yesod hiding (update)
import Network.Wai
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString as S
import Database.HXournal.IDMap.Type
import Data.Acid
import Data.Attoparsec as P
import Data.Aeson as A
import Data.UUID
import Data.Time.Clock
import Data.List (sortBy)
import Data.Function (on)
import Database.HXournal.IDMap.Server.Type

import qualified Data.ByteString.Lazy as LS
import Data.Text (unpack)
import Text.Blaze
import System.FilePath
import System.Directory 

import Control.Concurrent
import Database.HXournal.Store.Job

mkYesod "HXournalIDMapServer" [parseRoutes|
/ HomeR GET
/test TestR
/listhxournalidmap  ListHXournalIDMapR GET
/uploadhxournalidmap  UploadHXournalIDMapR POST
/hxournalidmap/#UUID HXournalIDMapR 
/listhxournalidmapusingtime/#UTCTime/#UTCTime ListHXournalIDMapUsingTimeR GET
/replacecreationtime/#UUID ReplaceCreationTimeR GET
/replacefile/#UUID ReplaceFileR POST 
|]


instance Yesod HXournalIDMapServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000

instance RenderMessage HXournalIDMapServer FormMessage where
  renderMessage _ _ = defaultFormMessage


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

getListHXournalIDMapUsingTimeR :: UTCTime -> UTCTime -> Handler RepHtmlJson 
getListHXournalIDMapUsingTimeR time1 time2 = do 
  liftIO $ putStrLn "getListHXournalIDMapUsingTimeR"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  liftIO $ putStrLn $ show time1 
  liftIO $ putStrLn $ show time2 
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Methods" "POST, GET"
  setHeader "X-Requested-With" "XmlHttpRequest"
  setHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type"
  let cmpfunc x = let ctime = hxournal_idmap_creationtime x
                  in ctime >= time1 && ctime <= time2
  let filtered = sortBy (compare `on` hxournal_idmap_creationtime) . filter cmpfunc $  r 
  defaultLayoutJson defhlet (A.toJSON (Just filtered))

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
  liftIO .  putStrLn . show $ bs 
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



handleTestR :: Handler RepHtmlJson
handleTestR = do 
  wr <- return . reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> liftIO $ putStrLn "GET called in TestR"
    "PUT" -> liftIO $ putStrLn "PUT called in TestR"
    _ -> liftIO $ putStrLn "???"
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Methods" "POST, GET"
  setHeader "X-Requested-With" "XmlHttpRequest"
  setHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type"
  defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe HXournalIDMapInfo))


getReplaceCreationTimeR :: UUID -> Handler RepHtmlJson
getReplaceCreationTimeR uuid = do 
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Methods" "*"
  setHeader "X-Requested-With" "XmlHttpRequest"
  setHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type"

  liftIO $ putStrLn "getReplaceCreationTimeR called"

  let defaultfn = defaultLayoutJson defhlet (A.toJSON ("test" :: String))


  acid <- return.server_acid =<< getYesod
  minfo <- liftIO $ query acid (QueryHXournalIDMap uuid)
  case minfo of 
    Nothing -> defaultfn
    Just info -> do 
      lst <- return . reqGetParams =<< getRequest 
      let lst2 =filter (\(x,y) -> x == "creationtime") lst 
          newinfo = if not . Prelude.null $ lst2
                    then let mutctime = fromSinglePiece . snd . head $ lst2
                         in maybe info (\utctime -> info { hxournal_idmap_creationtime = utctime }) mutctime 
                    else info 
      liftIO $ putStrLn $ show $ newinfo 
      r <- liftIO $ update acid (UpdateHXournalIDMap newinfo) 
      liftIO $ putStrLn $ show r 
      defaultfn 



detailIDMapInfoHamlet :: String 
                      -> GGWidget HXournalIDMapServer Handler ()
                      -> Enctype
                      -> HXournalIDMapInfo 
                      -> GGWidget HXournalIDMapServer Handler ()
detailIDMapInfoHamlet urlbase widget enctype  HXournalIDMapInfo{..} = do 
  setTitle (toHtml hxournal_idmap_uuid)
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.min.js"
  addStylesheetRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css"
  toWidget [julius|
$(function() { 
    $(".datepicker").datepicker();
    $("#form").submit(function(event){
	event.preventDefault();

        var creationtime = $('input[id="creationtime"]',this).val();
        var creationtime1 = creationtime.replace("/","").replace("/","") ;
        creationtime = creationtime1.substring(4,8)+creationtime1.substring(0,4)  
                   +"-000000-GMT";
        var theurl = "http://susy.physics.lsa.umich.edu:8090/idmap" + "@{ReplaceCreationTimeR hxournal_idmap_uuid}";

        console.debug(theurl);

        var params = { format : 'json' , "creationtime" : creationtime } 
        $.getJSON ( theurl, params );  
        window.location.href = "http://susy.physics.lsa.umich.edu:8090/idmap" + "@{HXournalIDMapR hxournal_idmap_uuid}"
    });
}); 

|]
  [whamlet|
    <h1> #{hxournal_idmap_uuid}
    <p>  #{hxournal_idmap_name}
    <p> #{hxournal_idmap_creationtime} 
      <form id="form" action=@{HXournalIDMapR hxournal_idmap_uuid}> 
        <input type="text" class="datepicker" id="creationtime">
        <input type="submit" value="Submit">
    <p>  #{hxournal_idmap_currentversion}
    <p>  #{hxournal_idmap_numofpages}
    <p> 
      <form id="formpost"  method=post action=#{urlbase}@{ReplaceFileR hxournal_idmap_uuid} enctype=#{enctype}>
        ^{widget}
        <input type="submit" value="Submit">
|]      



instance ToHtml UTCTime where
  toHtml = toHtml . show


idmapurlbase = "http://susy.physics.lsa.umich.edu:8090/idmap"

getHXournalIDMapR :: UUID -> Handler RepHtmlJson
getHXournalIDMapR idee = do 
  ((_,widget),enctype) <- generateFormPost fileForm
  liftIO $ putStrLn "getHXournalIDMapR called"
  acid <- return.server_acid =<< getYesod
  minfo <- liftIO $ query acid (QueryHXournalIDMap idee)
  liftIO $ putStrLn $ show minfo
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Methods" "POST, GET"
  setHeader "X-Requested-With" "XmlHttpRequest"
  setHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type"
  maybe (defaultLayoutJson defhlet (A.toJSON (Just minfo)))
        (\info->defaultLayoutJson (detailIDMapInfoHamlet idmapurlbase widget enctype info) (A.toJSON (Just minfo)))
        minfo


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



data FileForm = FileForm { fileFile :: FileInfo }
     deriving Show 

-- fileAForm :: (RenderMessage s FormMessage, RenderMessage m FormMessage) => AForm s m FileForm
fileAForm = FileForm <$> fileAFormReq "replace xoj file:"

fileForm = renderTable fileAForm 



postReplaceFileR :: UUID -> Handler RepHtmlJson 
postReplaceFileR idee = do 
  liftIO $ putStrLn "postReplaceFileR called"
  let defaultfn = defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe Int))
      

  acid <- return . server_acid =<< getYesod
  minfo <- liftIO $ query acid (QueryHXournalIDMap idee)
  maybe defaultfn 
        (\info -> do 
          let idstr = toString idee 
              nver = hxournal_idmap_currentversion info + 1
          ((result,widget),enctype) <- runFormPost fileForm 
          case result of 
            FormSuccess r -> do 
              let content = Yesod.fileContent . fileFile $ r
              tempdir <- liftIO $ getTemporaryDirectory
              let filename = (tempdir</> idstr ++ "_new.xoj")
              liftIO . LS.writeFile filename $ content
              liftIO $ threadDelay 1000000 
              npages <- liftIO $ startNewVersion idstr nver filename
              let newinfo = info { hxournal_idmap_currentversion = nver
                                 , hxournal_idmap_numofpages = npages } 
              liftIO $ update acid (UpdateHXournalIDMap newinfo)

              defaultfn 
            _ -> do 
              liftIO $ putStrLn "fail"
              defaultfn 
              

        ) 
        minfo



