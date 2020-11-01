{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module WebTemplates where 
import qualified Data.Text as T 
import qualified Data.Map as M
import qualified Text.Mustache as TM


selectTemplate = compileMustacheDir "webtemplates" "index.html"