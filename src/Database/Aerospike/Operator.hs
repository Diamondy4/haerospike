{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Database.Aerospike.Operator where

import Data.ByteString qualified as BS
import Database.Aerospike.Value (Value)

newtype ReadOp = ReadOp
    { binName :: BS.ByteString
    }
    deriving stock (Show, Eq)

data WriteOp = WriteOp
    { binName :: BS.ByteString
    , value :: Value
    }
    deriving stock (Show, Eq)

data TTL
    = DefaultTTL
    | NoExpireTTL
    | NoChangeTTL
    | ClientDefaultTTL
    | ManualSecs Int
    deriving stock (Show, Eq)

data Operator
    = Read ReadOp
    | Write WriteOp
    | Touch
    | Modify
    | SetTTL TTL
    | Delete
    deriving stock (Show, Eq)
