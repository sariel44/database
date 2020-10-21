{-#LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Model where

import qualified Data.Text as T
import qualified Data.Set as S
import Data.FuzzySet
import Data.FuzzySet.Types
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Aeson.TH

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Except
 
newtype Secret = Secret { secret :: B.ByteString }  

data Record = Record { 
      fuzzy :: M.Map T.Text FuzzySet
    , recordName :: String
    , keywords :: S.Set T.Text
    , tags :: S.Set T.Text
    , text :: T.Text
}

data Env = Env {
    currentRecordName :: String,
    currentDatabasePath :: FilePath
}

emptyRecord :: Record
emptyRecord = Record M.empty "" S.empty S.empty "" 

newtype DBMonad a = DBMonad { runDBMonad :: ReaderT Env (ExceptT String IO) a}
        deriving (Monad, Functor, Applicative, MonadIO, MonadError String, MonadReader Env)


$(deriveJSON defaultOptions ''Record)
$(deriveJSON defaultOptions ''FuzzySet)
$(deriveJSON defaultOptions ''GramInfo)
$(deriveJSON defaultOptions ''FuzzySetItem)
