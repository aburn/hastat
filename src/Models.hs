{-# LANGUAGE
    OverloadedStrings
  , EmptyDataDecls
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , GADTs
  , TypeFamilies
  , TemplateHaskell
  , QuasiQuotes
  , FlexibleInstances
  , FlexibleContexts
  , StandaloneDeriving #-}

module Models
    ( entityDefs
    , Trend(..)
    )
where

-- Needed for encoding and decoding to/from JSON

import           GHC.Generics
import           Data.Aeson
import           Data.Default.Class

-- Needed for generating our bookmark entity

import           Database.Persist
import           Database.Persist.Class
import           Database.Persist.TH

import           Control.Monad                  ( mzero )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )

share  [mkPersist sqlSettings, mkSave "entityDefs"][persistLowerCase|
    Trend
      name         Text
      url          Text
      tweet_volume Int Maybe
      UniqueN name
      deriving Show Generic
|]

instance ToJSON Trend where
    toJSON (Trend name url tv) =
        object ["name" .= name, "url" .= url, "tweet_volume" .= tv]

instance FromJSON Trend where
    parseJSON (Object t) =
        Trend <$> t .: "name" <*> t .: "url" <*> t .:? "tweet_volume"

    parseJSON _ = mzero
