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
import Control.Monad.Reader
import Control.Monad.Error

import qualified Data.Vector as V
import qualified Data.ByteString as B

import Data.Monoid 
import System.Directory
import System.IO
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Word

import Crypto.KDF.PBKDF2
import Crypto.Cipher.AES (AES256)
import Crypto.Random.EntropyPool


data DBOp = PutKeyWord T.Text
           | PutTag T.Text
           | PutText T.Text 
           | TableName String
    deriving (Show, Read)
          
version :: (Word8,Word8,Word8)
version = (0,0,1)

magic :: B.ByteString
magic = "shitfuck"

salt :: B.ByteString
salt = "sadhau389u32kdjnasod89as8afn923h8"

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
    encryptOps :: MonadError String m => Secret -> [o] -> m B.ByteString

class OpsDecrypter (m :: * -> *) o where 
    decryptOps :: MonadError String m => Secret -> B.ByteString -> m [o]


class GetBits m where
    getBits :: Int -> m B.ByteString


{-- DBMonad --}

newtype DBMonad a = DBMonad { runDBMonad :: ReaderT EntropyPool (StateT Database (ErrorT String IO)) a}
        deriving (Monad, Functor, Applicative, MonadIO, MonadState Database, MonadError String, MonadReader EntropyPool)


instance GetBits DBMonad where 
    getBits n = do
        et <- ask
        liftIO $ getEntropyFrom et n 

        

instance FileLister DBMonad where 
    listFiles fp = liftIO (getDirectoryContents fp)

instance FileSaver DBMonad where 
    saveFile fp = liftIO . B.writeFile fp 

parameters = Parameters 4000 256

instance OpsCrypter DBMonad DBOp where
    encryptOps s bs = do
            let bs = runPut printer
            return bs

        where printer :: Put 
              printer = do
                putByteString magic
                putWord8 major 
                putWord8 minor 
                putWord8 patch 
                putWord8 0
                putByteString (encodeOps bs)

              (major,minor,patch) = version

encodeOps :: [DBOp] -> B.ByteString
encodeOps = undefined

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