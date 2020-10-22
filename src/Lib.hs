{-# LANGUAGE KindSignatures, MultiParamTypeClasses, ViewPatterns, FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Lib
    ( Loader(..) 
    , Saver(..)
    , listDB
    , Indexer(..) 
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
import qualified Data.ByteString.Lazy as L

import Data.Monoid 
import System.Directory
import System.IO
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.List
import qualified Data.Map as M 

import Model
import Text


class Loader a where
    load :: FilePath -> DBMonad a

class Saver a where
    save :: a -> DBMonad ()

class Indexer a where 
    buildIndex :: a -> a

instance Loader Task where 
    load taskname = do  
        dbpath <- asks currentDatabasePath
        let fp = dbpath <> "/" <> taskname
        content <- liftIO $ B.readFile fp
        case decodeStrict content of 
            Nothing -> throwError $ "Couldn't decode bytestring for task " <> taskname
            Just a -> return a

instance Saver Task where
    save task = do 
            dbpath <- asks currentDatabasePath
            let fp = dbpath <> "/" <> taskName task <> ".tmp"
            let fpnew = dbpath <> "/" <> taskName task 
            liftIO $ L.writeFile fp $ encodePretty task
            liftIO $ renameFile fp fpnew 

   
instance Indexer Task where 
    buildIndex r1 = Task (buildFuzzySet (S.toList $ wrds `S.union` tgs)) (taskName r1)  (description r1) wrds tgs (status r1)   
        where wrds = filterWords (description r1 <> " ") 20
              tgs =  tagsTask r1


{-- Records --}
instance Loader Record where 
    load recordname = do 
        dbpath <- asks currentDatabasePath
        let fp = dbpath <> "/" <> recordname 
        content <- liftIO $ B.readFile fp 
        case decodeStrict content of 
            Nothing -> throwError $ "Couldn't decode bytestring for record " <> recordname
            Just a -> return a

instance Saver Record where 
    save record = do 
            dbpath <- asks currentDatabasePath
            let fp = dbpath <> "/" <> recordName record <> ".tmp"
            let fpnew = dbpath <> "/" <> recordName record 
            liftIO $ L.writeFile fp $ encodePretty record
            liftIO $ renameFile fp fpnew 


listDB :: DBMonad [String]
listDB = do
    dbpath <- asks currentDatabasePath
    xs <- liftIO $ getDirectoryContents dbpath
    return $ filter (not . ("." `isPrefixOf`)) xs

instance Indexer Record where 
    buildIndex r1 = Record (buildFuzzySet (S.toList $ wrds `S.union` tgs `S.union` mts)) (recordName r1) wrds tgs (text r1) (metadata r1)
        where   wrds = filterWords (text r1) 20
                tgs =  tags r1
                mts = S.fromList $ squashKeyValues $ M.toList $ metadata r1 
                squashKeyValues = map (\(k,v) -> k <> "-" <> v)

