module Lib
    ( makeIndex
    ) where

import qualified Data.Map.Strict as M 
import Data.FuzzySet
import Data.Text
import Control.Monad


data DBOp = PutKeyWord Text
           | PutTag Text
           | PutText Text 
    deriving (Show, Read)
          

data TableFile = TableFile {
      table_name :: String
    , index_name :: String
    } 

newtype Secret = Secret { secret :: Text }  

data Table = Table { 
      fuzzy :: M.Map Text FuzzySet
    , keywords :: [Text]
    , tags :: M.Map Text [Text]
    , text :: Text
}

data Database = Database {
      path :: String
    , databaseSecret :: Secret
    , tables :: M.Map String Table 
} 

class FileLoader m where 
    loadFile :: String -> m Text 

class FileLister m where 
    listFiles :: String -> m [String]

{-- 
    Toplevel API 
--}
saveDatabase :: Database -> IO ()
saveDatabase d = undefined

loadDatabase :: (Monad m, FileLoader m, FileLister m) => Secret -> String -> m Database 
loadDatabase s p = (Database p s . M.fromList) <$> tables 
    where tables = listTables p >>= \xs -> forM xs (loadTable s)

listTables :: FileLister m => String -> m [String]
listTables p = undefined

loadTable :: FileLoader m => Secret-> String -> m (String,Table) 
loadTable = undefined

makeIndex = undefined