{-# LANGUAGE ScopedTypeVariables #-}

module Database.Aerospike.Record where

import Data.ByteString qualified as BS
import Data.Word (Word16, Word32)
import Database.Aerospike.Value

data Record = MkRecord
    { gen :: Word16
    -- ^ Generation of the record. Updated each time modification performed on record.
    , ttl :: Word32
    -- ^ The time-to-live (expiration) of the record in seconds.
    , bins :: [(BS.ByteString, Value)]
    }
    deriving stock (Show, Eq, Ord)
