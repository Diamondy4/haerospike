{-# LANGUAGE ScopedTypeVariables #-}

module Database.Aerospike.Record where

import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Database.Aerospike.Value

-- FIXME: correct integer types for gen and ttl
data Record = MkRecord
    { gen :: Int
    , ttl :: Int
    , bins :: [(BS.ByteString, Value)]
    }
    deriving stock (Show, Eq)
