{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module WebLib where

import qualified Shelly as S
import qualified Data.Text as T 
import Control.Monad.Trans


import Web.Scotty
import Model as M
import Lib as L 


main :: IO ()
main = scotty 3000 $ do 

    get "/db" $ do
        xs <- liftIO $ S.shelly $ T.lines <$> S.run "ls" ["../templates/"] 
        json xs

    get "/db/:database/" $ do 
        name <- param "database"
        xs <- liftIO $ evalDBMonad (listDB) (Env name "")
        json xs


    get "/show/:database/:name" $ do 
        database <- param "database"
        name <- param "word"
        html $ "<h1>Reading " <> database <> " and " <> name <> "</h1>"