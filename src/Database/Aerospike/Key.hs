{-# LANGUAGE ScopedTypeVariables #-}

module Database.Aerospike.Key where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)

data PKey
    = KInteger Int64
    | KString Text
    | KBytes ByteString
    deriving (Show)

class ToKey a where
    toKey :: a -> PKey

-- TODO: Actual size of namespace and set is limited to fixed number of bytes,
-- need to somehow represent it in API
data Key = MkKey
    { namespace :: ByteString
    , set :: ByteString
    , pKey :: PKey
    }
