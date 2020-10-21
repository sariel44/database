{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module CLI where
import Lib
import qualified Model as M
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import qualified Data.FuzzySet as F
import System.IO
import System.Environment
import Data.Maybe
import Data.Aeson as A
import Data.Aeson.Encode.Pretty

usage :: String -> IO ()
usage err = putStrLnErr "usage:" 
    *> putStrLnErr "<program> get [database] <recordname>" 
    *> putStrLnErr "<program> save [database] <recordname> <tagfile>" 
    *> putStrLnErr "<program> search [database] <searchparam>" 
    *> error err

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr


modeSwitch :: IO (String, [String])
modeSwitch = do 
    xs <- getArgs 
    when (length xs < 2) $ usage "Not enough parameters"
    let action = head xs
    return (action, drop 1 xs)


main :: IO ()
main = do 
    mode <- modeSwitch  
    case mode of
        ("meta", [db,record,meta]) -> do 
                        flip M.evalDBMonad (M.Env db record) $ do
                            rc <- loadRecord record 
                            let mts = M.metadata rc
                            mts2 <- liftIO $ L.readFile meta 
                            let b = A.decode mts2
                            when (isNothing b) $ liftIO $ usage "You have a typo in your meta file" 
                            saveRecord $ buildIndexes $ rc {M.metadata = mts <> fromJust b}
        ("search", [db,search]) -> do 
                            xs <- M.evalDBMonad (do 
                                xs <- listRecords
                                forM xs loadRecord) (M.Env db "")
                            forM_ xs $ \x -> do 
                                let fz = M.fuzzy x
                                case F.get fz (T.pack search) of
                                    [] -> pure ()
                                    _ -> putStrLn $ M.recordName x <> " matches"   
                                 
        ("search",xs) -> usage "Wrong number of arguments for search"
        ("save", [db,record, tags]) -> do  
                        rs <- B.readFile record
                        ts <- L.readFile tags
                        putStrLnErr $ "Saving record in " <> db <> "/" <> record
                        let b = A.decode ts
                        when (isNothing b) $ usage "You have a typo in the tag file" 
                        M.evalDBMonad (saveRecord $ buildIndexes $ M.emptyRecord {M.recordName = record, M.text = T.decodeUtf8 rs, M.tags = fromJust $ b}) (M.Env db record)
        ("save",xs) -> usage "Not correct number of parameters for save"
        ("get", [db,record]) -> do 
                        r <- M.evalDBMonad (loadRecord record) (M.Env db record)
                        putStrLnErr  $ M.recordName r <> " tags dumped in " <> M.recordName r <> ".tags"
                        putStrLnErr  $ M.recordName r <> " keywords dumped in " <> M.recordName r <> ".keywords"
                        putStrLnErr  $ M.recordName r <> " fuzzy dumped in " <> M.recordName r <> ".fuzzy"
                        putStrLnErr  $ M.recordName r <> " meta dumped in " <> M.recordName r <> ".meta"
                        putStrLnErr  $ M.recordName r <> " text dumped in " <> M.recordName r
                        L.writeFile (M.recordName r <> ".tags") (encodePretty $ M.tags r) 
                        L.writeFile (M.recordName r <> ".keywords") (encodePretty $ M.keywords r) 
                        L.writeFile (M.recordName r <> ".fuzzy") (encodePretty $ M.fuzzy r) 
                        L.writeFile (M.recordName r <> ".meta") (encodePretty $ M.metadata r) 
                        B.writeFile (M.recordName r) (T.encodeUtf8 $ M.text r) 
        ("get", xs) -> usage "Wrong number of arguments for get" 
        (cmd, xs) -> usage $ "Not correct command " ++ cmd



        
    
