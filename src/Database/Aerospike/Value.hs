{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Aerospike.Value where

import Data.Bifunctor (Bifunctor (..))
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

instance FromValue Bool where
    fromValue = \case
        VBoolean v -> Just v
        _ -> Nothing

instance FromValue Int64 where
    fromValue = \case
        VInteger v -> Just v
        _ -> Nothing

instance FromValue Text where
    fromValue = \case
        VString v -> Just v
        _ -> Nothing

instance (FromValue a) => FromValue (V.Vector a) where
    fromValue = \case
        VList v -> V.forM v fromValue
        _ -> Nothing

instance (Ord k, FromValue k, FromValue v) => FromValue (Map.Map k v) where
    fromValue = \case
        VMap v -> do
            let parsePair (k, v) = do
                    k' <- fromValue @k k
                    v' <- fromValue @v v
                    pure (k', v')

            pairs <- traverse parsePair . Map.assocs $ v
            pure $ Map.fromList pairs
        _ -> Nothing

instance FromValue BS.ByteString where
    fromValue = \case
        VBytes v -> Just v
        _ -> Nothing

class ToValue a where
    toValue :: a -> Value

instance ToValue Bool where
    toValue = VBoolean

instance ToValue Int64 where
    toValue = VInteger

instance ToValue Text where
    toValue = VString

instance (ToValue a) => ToValue (V.Vector a) where
    toValue = VList . V.map toValue

instance (ToValue k, ToValue v) => ToValue (Map.Map k v) where
    toValue = VMap . Map.fromList . fmap (bimap toValue toValue) . Map.assocs

instance ToValue BS.ByteString where
    toValue = VBytes
