{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module WebLib where

import qualified Shelly as S
import qualified Data.Text as T 
import Control.Monad.Trans
import Text.Blaze.Html.Renderer.Text (renderHtml)



import Control.Monad.Except
import Web.Scotty
import qualified Model as M
import Lib as L 
import qualified Data.Set as S

import Text.Julius
import Text.Hamlet
import WebTemplates

data TextBody = TextBody {
    body :: T.Text,
    tags :: S.Set T.Text
}

key :: [(T.Text, [String])] -> [T.Text]
key xs = join $ map f xs 
    where f (db,tbls) = foldr step [] tbls
            where step x z = (db <> "/" <> T.pack x):z 

main :: IO ()
main = scotty 3000 $ do 


    get "/" $ do 
        html $  renderHtml (indexHTML defaultRender) 


    get "/db" $ do
        xs <- liftIO $ S.shelly $ T.lines <$> S.run "ls" ["templates/"] 
        json xs

    get "/db/:database" $ do 
        name <- param "database"
        xs <- liftIO $ M.evalDBMonad (listDB) (M.Env name "")
        json xs

    get "/db/:database/:name" $ do
        database <- param "database"
        name <- param "name"
        xs <- liftIO $ M.evalDBMonad (load name :: M.DBMonad M.Record) (M.Env database name)
        json xs

    get "/search/:word" $ do 
        dbs <- liftIO $ S.shelly $ T.lines <$> S.run "cat" ["databases"] 
        word <- param "word"
        results <- forM dbs $ \db -> do 
            result <- liftIO $ M.evalDBMonad (search word) (M.Env (T.unpack db) "")
            return (db, M.recordName <$> result)
        
        json $ key  results


    put "/task/:database/:name" $ do
        database <- param "database"
        name <- param "name"
        (M.TextBody body tags) <- jsonData
        liftIO $ M.evalDBMonad (save $ buildIndex $ M.emptyTask {M.taskName = name, M.description = body, M.tagsTask = tags}) (M.Env database name)

    post "/task/:database/:name/:status" $ do 
        database <- param "database"
        name <- param "name"
        status <- param "status"
        liftIO $ do 
            flip M.evalDBMonad (M.Env database name) $ do 
                t <- load name
                case status of 
                    "open" -> save $ buildIndex $ t { M.status = M.Open} 
                    "busy" -> save $ buildIndex $ t { M.status = M.Busy} 
                    "done" -> save $ buildIndex $ t { M.status = M.Done} 
                    x -> throwError $ "Invalid status: " <> x 

    put "/db/:database/:name" $ do 
        database <- param "database"
        name <- param "name"
        (M.TextBody body tags) <- jsonData  
        liftIO $ M.evalDBMonad (save $ buildIndex $ M.emptyRecord {M.recordName = name, M.text = body, M.tags = tags}) (M.Env database name)
        json "ok"
        
    get "/js/main.js" $ do 
        addHeader "content-type" "text/javascript"
        text $ renderJavascript (mainJS defaultRender) 