{-# LANGUAGE KindSignatures, MultiParamTypeClasses, ViewPatterns, FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Lib
    ( loadRecord 
    , saveRecord 
    , listRecords
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
import qualified Data.Set as S

import qualified Data.Vector as V
import qualified Data.ByteString as B

import Data.Monoid 
import System.Directory
import System.IO
import Data.Aeson

import Model
import Text

{-- DBMonad --}

loadRecord :: FilePath -> DBMonad Record
loadRecord recordname = do 
    dbpath <- asks currentDatabasePath
    let fp = dbpath <> "/" <> recordname 
    content <- liftIO $ B.readFile fp 
    case decodeStrict content of 
        Nothing -> throwError $ "Couldn't decode bytestring for record " <> recordname
        Just a -> return a

saveRecord :: Record -> DBMonad ()
saveRecord record = do 
        dbpath <- asks currentDatabasePath
        let fp = dbpath <> "/" <> recordName record <> ".tmp"
        let fpnew = dbpath <> "/" <> recordName record 
        liftIO $ encodeFile fp record
        liftIO $ renameFile fp fpnew 


listRecords :: DBMonad [String]
listRecords = do
    dbpath <- asks currentDatabasePath
    liftIO $ getDirectoryContents dbpath


buildIndexes :: Record -> Record
buildIndexes r1 = Record (buildFuzzySet (S.toList $ wrds `S.union` tgs)) (recordName r1) wrds tgs (text r1)
    where wrds = filterWords (text r1) 20
          tgs =  tags r1


appendToRecord :: Record -> Record -> DBMonad ()
appendToRecord r1 r2 | recordName r1 == recordName r2 = saveRecord $ buildIndexes newRecord 
        where newRecord = r2 {keywords = keywords r1 <> keywords r2, tags = tags r1 <> tags r2, text = text r1 <> "\n" <> text r2}
appendToRecord r1 r2 = throwError $ "Record name is not the same " <> recordName r1 <> " /= " <> recordName r2