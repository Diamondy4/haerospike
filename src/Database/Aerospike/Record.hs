{-# LANGUAGE ScopedTypeVariables #-}

module Database.Aerospike.Record where

import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word16, Word32)
import Database.Aerospike.Value

-- FIXME: correct integer types for gen and ttl
data Record = MkRecord
    { gen :: Word16
    , ttl :: Word32
    , bins :: [(BS.ByteString, Value)]
    }
    deriving stock (Show, Eq)
