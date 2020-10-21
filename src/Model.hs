{-#LANGUAGE TemplateHaskell #-}
module Model where

import qualified Data.Text as T
import Data.FuzzySet
import Data.FuzzySet.Types
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Aeson.TH
 

newtype Secret = Secret { secret :: B.ByteString }  

data Database = Database {
      path :: String
    , databaseSecret :: Secret
    , tables :: M.Map String Table 
} 

data Table = Table { 
      fuzzy :: M.Map T.Text FuzzySet
    , tableName :: String
    , keywords :: [T.Text]
    , tags :: M.Map T.Text Bool
    , text :: T.Text
    , tweak :: T.Text
}

$(deriveJSON defaultOptions ''Table)
$(deriveJSON defaultOptions ''FuzzySet)
$(deriveJSON defaultOptions ''GramInfo)
$(deriveJSON defaultOptions ''FuzzySetItem)
