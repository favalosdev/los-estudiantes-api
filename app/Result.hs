{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Result where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics ( Generic )
import Database.PostgreSQL.Simple.FromRow ( field, FromRow(..) )
import qualified Database.PostgreSQL.Simple.FromRow as Database.PostgreSQL.Simple.Internal

newtype Result = Result {averageScore :: Float} deriving (Show, Generic)

instance ToJSON Result
instance FromJSON Result

instance FromRow Result where
    fromRow :: Database.PostgreSQL.Simple.Internal.RowParser Result
    fromRow = Result <$> field