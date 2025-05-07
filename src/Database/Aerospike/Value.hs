{-# LANGUAGE ScopedTypeVariables #-}

module Database.Aerospike.Value where

import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V

-- | Aerospike bin value represented as Haskell value.
data Value
    = VNil
    | VBoolean Bool
    | VInteger Int64
    | VString Text
    | VList (V.Vector Value)
    | VMap (Map.Map Value Value)
    | VBytes BS.ByteString
    deriving stock (Show, Eq, Ord)

class FromValue a where
    fromValue :: Value -> Maybe a
