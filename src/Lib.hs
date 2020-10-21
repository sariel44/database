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
import Control.Monad.Error

import qualified Data.Vector as V
import qualified Data.ByteString as B

import Data.Monoid 
import System.Directory
import System.IO
import Data.Aeson.TH

import Model

import Crypto.KDF.PBKDF2
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
import  Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import  Crypto.Error (CryptoFailable(..), CryptoError(..))

import Crypto.Random.EntropyPool

          
salt :: B.ByteString
salt = "sadhau389u32kdjnasod89as8afn923h8"


emptyTable :: Table
emptyTable = Table M.empty "nothing" [] M.empty ""

class FileLoader m where 
    loadFile :: String -> m B.ByteString 

class FileLister m where 
    listFiles :: String -> m [String]

class FileSaver m where
    saveFile :: String -> B.ByteString -> m ()

class TableEncrypter (m :: * -> *) where
    encryptTable :: Secret -> Table -> m B.ByteString


class TableDecrypter (m :: * -> *) o where
    decryptTable :: Secret -> B.ByteString -> m Database


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

parameters :: Parameters
parameters = Parameters 4000 256

blockCipher :: AES256
blockCipher = undefined

deriveKey :: Secret -> B.ByteString
deriveKey s = fastPBKDF2_SHA512 parameters (secret s) salt  

instance TableEncrypter DBMonad where 
    encryptOps table = do
            let bs =
            let key = deriveKey s
            case cipherInit key of 
                CryptoFailed e -> throwError (show e)
                CryptoPassed (c :: AES256) -> do                    
                    iv <- makeIV <$> getBits  (blockSize c)
                    case iv of
                        Nothing -> throwError "No iv created"
                        Just iv -> return $ ctrCombine c iv bs 


encodeOps :: [DBOp] -> B.ByteString
encodeOps xs = runPut . mapM_ step 
    where 
        step (TableName nm) = do
            putWord8 3
            putByteString $ T.encodeUtf8 nm
        step (PutText txt) = do
            putWord8 2
            putByteString $ T.encodeUtf8 txt
        step (PutTag xs) = do
            putWord8 1
            putByteString $ T.encodeUtf8 xs
        step (PutKeyWord xs) = do
            putWord8 0
            putByteString $ T.encodeUtf8 xs



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