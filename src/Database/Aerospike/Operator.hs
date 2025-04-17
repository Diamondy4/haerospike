{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Database.Aerospike.Operator where

import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word32)
import Database.Aerospike.Value (Value)

data ReadOp
    = ReadSome [BS.ByteString]
    | ReadAll
    | ReadOne BS.ByteString
    deriving stock (Show, Eq)

data WriteOp = WriteOp
    { binName :: BS.ByteString
    , value :: Value
    }
    deriving stock (Show, Eq)

data ModifyOp
    = Incr Int64
    | RAppend BS.ByteString
    | RPrepend BS.ByteString
    | SAppend Text
    | SPrepend Text
    | LAppend (V.Vector Value)
    | MPut (Map.Map Value Value)
    deriving stock (Show, Eq)

data TTL
    = DefaultTTL
    | NoExpireTTL
    | NoChangeTTL
    | ClientDefaultTTL
    | ManualSecs Word32
    deriving stock (Show, Eq)

data Operator
    = Read ReadOp
    | Write WriteOp
    | Touch
    | Modify BS.ByteString ModifyOp
    | SetTTL TTL
    | Delete
    deriving stock (Show, Eq)
