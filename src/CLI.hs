{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module CLI where
import Lib
import qualified Model as M
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import System.IO
import System.Environment
import Data.Aeson.Encode.Pretty

data Mode = GetRecord {database :: String, record :: String}
          | SaveRecord {database :: String, record :: String, tags :: String} 
          | AppendRecord {database :: String, record :: String, tags :: String}
          | SearchRecords {database :: String, searchTag :: String}
    deriving (Show, Read)

usage :: String -> IO ()
usage err = putStrLnErr "usage:" 
    *> putStrLnErr "<program> [database] get <recordname>" 
    *> error err

putStrLnErr = hPutStrLn stderr

main :: IO ()
main = do 
    xs <- getArgs 
    when (length xs < 2) $ usage "Not enough parameters"
    let action = head xs 
    case action of
        "save"-> case drop 1 xs of
                    [db, record, tags] -> do
                        rs <- B.readFile record
                        ts <- B.readFile tags
                        putStrLnErr $ "Saving record in " <> db <> "/" <> record
                        M.evalDBMonad (saveRecord $ buildIndexes $ M.emptyRecord {M.text = T.decodeUtf8 rs, M.tags = S.fromList $ T.words $ T.decodeUtf8 ts}) (M.Env db record)

        "get" -> case drop 1 xs of
                    [db,record] -> do 
                        r <- M.evalDBMonad (loadRecord record) (M.Env db record)
                        putStrLnErr  $ M.recordName r <> " tags dumped in " <> M.recordName r <> ".tags"
                        putStrLnErr  $ M.recordName r <> " keywords dumped in " <> M.recordName r <> ".keywords"
                        putStrLnErr  $ M.recordName r <> " fuzzy dumped in " <> M.recordName r <> ".fuzzy"
                        putStrLnErr  $ M.recordName r <> " text dumped in " <> M.recordName r
                        L.writeFile (M.recordName r <> ".tags") (encodePretty $ M.tags r) 
                        L.writeFile (M.recordName r <> ".keywords") (encodePretty $ M.keywords r) 
                        L.writeFile (M.recordName r <> ".fuzzy") (encodePretty $ M.fuzzy r) 
                        B.writeFile (M.recordName r) (T.encodeUtf8 $ M.text r) 
                    xs -> usage "Wrong number of arguments for get" 



        
    
