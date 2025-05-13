{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Database.Aerospike.Operator where

import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word32)
import Database.Aerospike.Value (MapKey, Value)

-- | Perform Read operation on record
data ReadOp
    = -- | Read bins specified in the list. Only those bins will be loaded from server
      ReadSome [BS.ByteString]
    | -- | Read all bins related to given record
      ReadAll
    | -- | Read exactly one specified bin. Only this bin will be loaded from server
      ReadOne BS.ByteString
    deriving stock (Show, Eq)

-- | Perform write operation on record
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
    | MPut (Map.Map MapKey Value)
    deriving stock (Show, Eq)

data TTL
    = -- | Use the server default ttl from the namespace
      DefaultTTL
    | -- | Do not expire the record
      NoExpireTTL
    | -- | Keep the existing record ttl when the record is updated
      NoChangeTTL
    | -- | Use the default client ttl in as_policy_operate
      ClientDefaultTTL
    | -- | Manually set ttl in seconds
      ManualSecs Word32
    deriving stock (Show, Eq)

data Operator
    = Read ReadOp
    | Write WriteOp
    | -- | Update ttl.
      Touch
    | -- | Modify specified bin with operation. This modification will be visible to following
      -- | reads in same `operate` transaction.
      Modify BS.ByteString ModifyOp
    | -- | Set ttl of the record.
      SetTTL TTL
    | -- | Delete record. All read bins still will be obtained.
      Delete
    deriving stock (Show, Eq)
