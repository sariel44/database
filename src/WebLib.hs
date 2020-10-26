{-# LANGUAGE OverloadedStrings #-}
module WebLib where

import Web.Scotty


main = scotty 3000 $ do 
    get "/show/:database/:name" $ do 
        database <- param "database"
        name <- param "word"
        html $ "<h1>Reading " <> database <> " and " <> name <> "</h1>"