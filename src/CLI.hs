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
        ("task-save",[db,taskName,tags]) -> do
                        putStrLnErr $ "Saving record in " <> db <> "/" <> taskName
                        rs <- B.readFile taskName 
                        ts <- L.readFile tags
                        let b = A.decode ts
                        when (isNothing b) $ usage "You have a typo in the tag file" 
                        M.evalDBMonad (save $ buildIndex $ M.emptyTask {M.taskName = taskName, M.description = T.decodeUtf8 rs, M.tagsTask = fromJust $ b}) (M.Env db taskName)
        ("task-save",xs) -> usage "Wrong number of arguments for task creation"

        ("task-busy", [db,taskName]) -> do
                        M.evalDBMonad (do 
                            t <- load taskName
                            save $ buildIndex $  t {M.status = M.Busy}) (M.Env db taskName) 
        ("task-busy", xs) -> usage "Wrong number of arguments for setting task to busy"

        ("task-done", [db,taskName]) -> do
                        M.evalDBMonad (do 
                            t <- load taskName
                            save $ buildIndex $  t {M.status = M.Done}) (M.Env db taskName) 
        ("task-done", xs) -> usage "Wrong number of arguments for setting task to done"


        ("task-open", [db,taskName]) -> do
                        M.evalDBMonad (do 
                            t <- load taskName
                            save $ buildIndex $  t {M.status = M.Open}) (M.Env db taskName) 
        ("task-open", xs) -> usage "Wrong number of arguments for setting task to open"

        ("task-show", [db, task]) -> do
                        r <- M.evalDBMonad (load task) (M.Env db task) :: IO M.Task
                        B.putStrLn $ T.encodeUtf8 (M.description r)
                        L.putStrLn $ "tags: " <> encode (M.tagsTask r)
                        L.putStrLn $ "keywords: " <> encode (M.keywordsTask r)
                        L.putStrLn $ "status: " <> encode (M.status r)
        ("task-show", xs) -> usage "Wrong number of arguments for setting task to show"
        ("task-search", [db,search]) -> do 
                            xs <- M.evalDBMonad (do 
                                xs <- listDB
                                forM xs (load :: FilePath -> M.DBMonad M.Task)) (M.Env db "")
                            forM_ xs $ \x -> do 
                                    let fz = M.fuzzyTask x
                                    case F.get fz (T.pack search) of
                                        [] -> pure ()
                                        _ -> putStrLn $ M.taskName x <> " matches"   

        ("task-search", xs) -> usage "Wrong number of arguments for setting task search"
        ("task-get", [db,task]) -> do 
                        r <- M.evalDBMonad (load task) (M.Env db task) :: IO M.Task
                        putStrLnErr  $ M.taskName r <> " tags dumped in " <> M.taskName r <> ".tags"
                        putStrLnErr  $ M.taskName r <> " keywords dumped in " <> M.taskName r <> ".keywords"
                        putStrLnErr  $ M.taskName r <> " fuzzy dumped in " <> M.taskName r <> ".fuzzy"
                        putStrLnErr  $ M.taskName r <> " description dumped in " <> M.taskName r
                        L.writeFile (M.taskName r <> ".tags") (encodePretty $ M.tagsTask r) 
                        L.writeFile (M.taskName r <> ".keywords") (encodePretty $ M.keywordsTask r) 
                        L.writeFile (M.taskName r <> ".fuzzy") (encodePretty $ M.fuzzyTask r) 
                        B.writeFile (M.taskName r) (T.encodeUtf8 $ M.description r) 
        ("task-get", xs) -> usage "Wrong number of arguments for get" 

        ("meta", [db,record,meta]) -> do 
                        flip M.evalDBMonad (M.Env db record) $ do
                            rc <- load record :: M.DBMonad M.Record 
                            let mts = M.metadata rc
                            mts2 <- liftIO $ L.readFile meta 
                            let b = A.decode mts2
                            when (isNothing b) $ liftIO $ usage "You have a typo in your meta file" 
                            save $ buildIndex $ rc {M.metadata = mts <> fromJust b}
        ("search", [db,search]) -> do 
                            xs <- M.evalDBMonad (do 
                                xs <- listDB
                                forM xs (load :: FilePath -> M.DBMonad M.Record)) (M.Env db "")
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
                        M.evalDBMonad (save $ buildIndex $ M.emptyRecord {M.recordName = record, M.text = T.decodeUtf8 rs, M.tags = fromJust $ b}) (M.Env db record)
        ("save",xs) -> usage "Not correct number of parameters for save"
        ("show", [db, record]) -> do
                        r <- M.evalDBMonad (load record) (M.Env db record) :: IO M.Record
                        B.putStrLn $ T.encodeUtf8 (M.text r)
                        L.putStrLn $ "tags: " <> encode (M.tags r)
                        L.putStrLn $ "keywords: " <> encode (M.keywords r)
                        L.putStrLn $ "meta: " <> encode (M.metadata r)

        ("get", [db,record]) -> do 
                        r <- M.evalDBMonad (load record) (M.Env db record) :: IO M.Record
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



        
    
