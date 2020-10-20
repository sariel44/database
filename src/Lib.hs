module Lib
    ( makeIndex
    ) where
import           Proto3.Wire
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Wire.Decode as Decode
import qualified Data.Map.Strict as M 
import Data.FuzzySet

data DBFile = DBFile {
      db_name :: String
    , index_name :: String
    } 

newtype Secret = Secret { secret :: String }  

newtype RawIndex = RawIndex { unRawIndex :: M.Map String [String] }

newtype Indexes = Indexes { 
    unIndexes :: M.Map String (FuzzySet)
}

makeIndex = undefined