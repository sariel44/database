{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module WebLib where

import qualified Shelly as S
import qualified Data.Text as T 
import Control.Monad.Trans
import Text.Blaze.Html.Renderer.Text (renderHtml)



import Web.Scotty
import qualified Model as M
import Lib as L 

import Text.Hamlet

main :: IO ()
main = scotty 3000 $ do 

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

    get "/" $ do 
        html $ renderHtml (
            [shamlet| <h1>hello world</h1>
        |])  