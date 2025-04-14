{-# LANGUAGE ScopedTypeVariables #-}

module Database.Aerospike.Key where

import Data.ByteString (ByteString)
import Data.Text (Text)

data PKey
    = KInteger Int
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
