{-# LANGUAGE KindSignatures, MultiParamTypeClasses, ViewPatterns, FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Lib
    ( loadDatabase
    , saveDatabase

    ) where

import qualified Data.Map.Strict as M 
import Data.FuzzySet
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Vector as V
import qualified Data.ByteString as B

import Data.Monoid 
import System.Directory
import System.IO
import Data.Aeson

import Model

{-- DBMonad --}

loadRecord :: FilePath -> DBMonad Record
loadRecord recordname = do 
    dbpath <- asks currentDatabasePath
    let fp = dbpath <> "/" <> recordname <> ".json" 
    content <- liftIO $ B.readFile fp 
    case decodeStrict content of 
        Nothing -> throwError $ "Couldn't decode bytestring for record " <> recordname
        Just a -> return a

saveRecord :: Record -> DBMonad ()
saveRecord record = do 
        dbpath <- asks currentDatabasePath
        let fp = dbpath <> "/" <> recordName record <> ".tmp"
        let fpnew = dbpath <> "/" <> recordName record <> ".json"
        liftIO $ encodeFile fp record
        liftIO $ renameFile fp fpnew 

