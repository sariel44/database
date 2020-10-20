{-# LANGUAGE KindSignatures, MultiParamTypeClasses, ViewPatterns, FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}
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
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Monoid 
import System.Directory


data DBOp = PutKeyWord T.Text
           | PutTag T.Text
           | PutText T.Text 
           | TableName String
    deriving (Show, Read)
          

newtype Secret = Secret { secret :: B.ByteString }  

data Table = Table { 
      fuzzy :: M.Map T.Text FuzzySet
    , tableName :: String
    , keywords :: [T.Text]
    , tags :: M.Map T.Text Bool
    , text :: T.Text
}


emptyTable :: Table
emptyTable = Table M.empty "nothing" [] M.empty ""

data Database = Database {
      path :: String
    , databaseSecret :: Secret
    , tables :: M.Map String Table 
} 

class FileLoader m where 
    loadFile :: String -> m B.ByteString 

class FileLister m where 
    listFiles :: String -> m [String]

class FileSaver m where
    saveFile :: String -> B.ByteString -> m ()


class OpsCrypter (m :: * -> *) o where 
    encryptOps :: Secret -> [o] -> m B.ByteString

class OpsDecrypter (m :: * -> *) o where 
    decryptOps :: Secret -> B.ByteString -> m [o]

{-- DBMonad --}

newtype DBMonad a = DBMonad { runDBMonad :: StateT Database IO a}
        deriving (Monad, Functor, Applicative, MonadIO, MonadState Database)

instance FileLister DBMonad where 
    listFiles fp = liftIO (getDirectoryContents fp)

{-- 
    Toplevel API 
--}
saveDatabase :: (Monad m, FileSaver m) => Database -> m ()
saveDatabase d = forM_ ts (saveTable p s)
        where p = path d
              s = databaseSecret d 
              ts = tables d

saveTable :: FileSaver m => String -> Secret -> Table -> m () 
saveTable path secret table = undefined 

loadDatabase :: (OpsDecrypter m DBOp, Monad m, FileLoader m, FileLister m) => Secret -> String -> m Database 
loadDatabase s p = (Database p s . M.fromList) <$> tables 
    where tables = listTables p >>= \xs -> forM xs (loadTable s)

listTables :: FileLister m => String -> m [String]
listTables p = undefined


loadTable :: (FileLoader m, OpsDecrypter m DBOp, Monad m) => Secret -> String -> m (String, Table) 
loadTable s p = (,) <$> (tableName <$> rawTable) <*> rawTable
    where rawTable = interpretOps emptyTable <$> (decryptOps s =<< loadFile p) 

interpretOps :: Table -> [DBOp] -> Table
interpretOps tbl = foldr step tbl
    where step (PutKeyWord k) tbl@(keywords -> ks) = tbl{keywords = k:ks}
          step (PutTag t) tbl@(tags -> ks) = tbl{tags = M.insert t True ks }
          step (TableName nm) tbl = tbl {tableName = nm}
          step (PutText txt) tbl@(text -> txts) = tbl { text = txt `T.append` "\n" `T.append` txts }

makeIndex = undefined