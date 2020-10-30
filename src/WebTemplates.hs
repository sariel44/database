{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module WebTemplates where 
import Text.Hamlet
import Text.Julius
import qualified Data.Text as T 
import Text.Blaze
import qualified Data.Map as M

defaultRender :: r -> [(T.Text,T.Text)] -> T.Text
defaultRender _ _ = ""

indexHTML :: Render () -> Html
indexHTML = $(hamletFile  "webtemplates/index.haml")  

mainJS :: Render () -> Javascript
mainJS = $(jsFile "js/main.js")