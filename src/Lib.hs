module Lib
    ( makeIndex
    ) where

import qualified Data.Map.Strict as M 
import Data.FuzzySet
import qualified Data.Text as T
import Control.Monad
import qualified Data.Vector as V


data DBOp = PutKeyWord T.Text
           | PutTag T.Text
           | PutText T.Text 
    deriving (Show, Read)
          

newtype Secret = Secret { secret :: T.Text }  

data Table = Table { 
      fuzzy :: M.Map T.Text FuzzySet
    , tableName :: String
    , keywords :: [T.Text]
    , tags :: M.Map T.Text [T.Text]
    , text :: T.Text
}

data Database = Database {
      path :: String
    , databaseSecret :: Secret
    , tables :: M.Map String Table 
} 

class FileLoader m where 
    loadFile :: String -> m T.Text 

class FileLister m where 
    listFiles :: String -> m [String]

class FileSaver m where
    saveFile :: String -> T.Text -> m ()

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

loadDatabase :: (Monad m, FileLoader m, FileLister m) => Secret -> String -> m Database 
loadDatabase s p = (Database p s . M.fromList) <$> tables 
    where tables = listTables p >>= \xs -> forM xs (loadTable s)

listTables :: FileLister m => String -> m [String]
listTables p = undefined

loadTable :: FileLoader m => Secret-> String -> m (String,Table) 
loadTable = undefined

makeIndex = undefined