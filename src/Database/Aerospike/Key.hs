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

data Key = MkKey
    { namespace :: ByteString
    , set :: ByteString
    , pKey :: PKey
    }
