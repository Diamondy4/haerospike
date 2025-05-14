{-# LANGUAGE ScopedTypeVariables #-}

module Database.Aerospike.Key (
    PKey (..),
    ToKey (..),
    Namespace,
    mkNamespace,
    Set,
    mkSet,
    Key (..),
    namespaceBS,
    setBS,
)
where

import Control.Monad (guard)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)

data PKey
    = KInteger Int64
    | KString Text
    | KBytes BS.ByteString
    deriving (Show)

class ToKey a where
    toKey :: a -> PKey

instance ToKey Int64 where
    toKey = KInteger

instance ToKey Text where
    toKey = KString

instance ToKey BS.ByteString where
    toKey = KBytes

newtype Namespace = Namespace {bs :: BS.ByteString}

-- TODO: make version that take compile time checked Symbol
mkNamespace :: BS.ByteString -> Maybe Namespace
mkNamespace x = do
    guard (BS.length x < 32)
    pure $ Namespace x

namespaceBS :: Namespace -> BS.ByteString
namespaceBS = (.bs)

newtype Set = Set {bs :: BS.ByteString}

-- TODO: make version that take compile time checked Symbol
mkSet :: BS.ByteString -> Maybe Set
mkSet x = do
    guard (BS.length x < 64)
    pure $ Set x

setBS :: Set -> BS.ByteString
setBS = (.bs)

-- TODO: Actual size of namespace and set is limited to fixed number of bytes,
-- need to somehow represent it in API
data Key = MkKey
    { namespace :: Namespace
    , set :: Set
    , pKey :: PKey
    }
