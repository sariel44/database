{-#LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedLists #-}
module Model where

import qualified Data.Text as T
import qualified Data.Set as S
import Data.FuzzySet.Types
import Data.FuzzySet as F
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Aeson.TH

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Except
 
newtype Secret = Secret { secret :: B.ByteString }  

data Status = Open | Busy | Done

data Record = Record { 
      fuzzy :: FuzzySet
    , recordName :: String
    , keywords :: S.Set T.Text
    , tags :: S.Set T.Text
    , text :: T.Text
    , metadata :: M.Map T.Text T.Text
}

data Task = Task {
        fuzzyTask :: FuzzySet
      , taskName :: String
      , description :: T.Text 
      , keywordsTask :: S.Set T.Text
      , tagsTask :: S.Set T.Text
      , status :: Status
}

data Env = Env {
    currentDatabasePath :: FilePath,
    currentRecordName :: String
}


data TextBody = TextBody {
    textBody :: T.Text,
    textTags :: S.Set T.Text
}


emptyTask :: Task
emptyTask = Task (F.emptySet 0 0 False) "" "" S.empty S.empty Open

emptyRecord :: Record
emptyRecord = Record (F.emptySet 0 0 False) "" S.empty S.empty "" M.empty 

newtype DBMonad a = DBMonad { runDBMonad :: ReaderT Env (ExceptT String IO) a}
        deriving (Monad, Functor, Applicative, MonadIO, MonadError String, MonadReader Env)


evalDBMonad :: DBMonad a -> Env -> IO a
evalDBMonad m env = do 
    xs <- runExceptT $ runReaderT ( runDBMonad m) env 
    case xs of
        Left e -> error e
        Right a -> return a

$(deriveJSON defaultOptions ''Record)
$(deriveJSON defaultOptions ''FuzzySet)
$(deriveJSON defaultOptions ''GramInfo)
$(deriveJSON defaultOptions ''FuzzySetItem)
$(deriveJSON defaultOptions ''Status)
$(deriveJSON defaultOptions ''Task)
$(deriveJSON defaultOptions ''TextBody)