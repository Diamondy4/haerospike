{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Aerospike.Record (
    BinName,
    binNameBS,
    mkBinName,
    Record (..),
    FromAsBins (..),
    ToAsBins (..),
    RawBins,
) where

import Data.ByteString qualified as BS hiding (pack)
import Data.ByteString.Char8 qualified as BS
import Data.Map qualified as M
import Data.Word (Word16, Word32)
import Database.Aerospike.Value (
    FromValue (..),
    ToValue (..),
    Value,
 )

import Control.Monad (guard)
import Data.Kind (Constraint)
import Data.Maybe (fromJust)
import GHC.Generics (Generically (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Generics.SOP (
    All,
    ConstructorInfo (Record),
    DatatypeInfo (ADT, Newtype),
    FieldInfo (..),
    Generic (..),
    HCollapse (hcollapse),
    HasDatatypeInfo (..),
    I (..),
    K (..),
    NP (Nil, (:*)),
    NS (Z),
    Proxy (Proxy),
    SOP (SOP),
    unSOP,
    unZ,
 )
import Generics.SOP.NP (cmap_NP, czipWith_NP, sequence_NP)
import Generics.SOP.Type.Metadata qualified as Meta

newtype BinName = BinName {bs :: BS.ByteString}
    deriving stock (Eq, Show, Ord)

binNameBS :: BinName -> BS.ByteString
binNameBS = (.bs)

mkBinName :: BS.ByteString -> Maybe BinName
mkBinName x = do
    guard (BS.length x < 16)
    pure $ BinName x

type RawBins = [(BinName, Value)]

-- TODO: actually max bin name size is limited to 15, somehow represent it
data Record a = MkRecord
    { gen :: Word16
    -- ^ Generation of the record. Updated each time modification performed on record.
    , ttl :: Word32
    -- ^ The time-to-live (expiration) of the record in seconds.
    , bins :: a
    }
    deriving stock (Show, Eq, Ord)

class FromAsBins a where
    fromAsBins :: [(BinName, Value)] -> Maybe a

class ToAsBins a where
    toAsBins :: a -> [(BinName, Value)]

instance FromAsBins [(BinName, Value)] where
    fromAsBins = Just

instance ToAsBins [(BinName, Value)] where
    toAsBins = id

type family ConstructorInfos (info :: Meta.DatatypeInfo) :: [Meta.ConstructorInfo] where
    ConstructorInfos ('Meta.ADT _ _ cons _) = cons
    ConstructorInfos ('Meta.Newtype _ _ con) = '[con]

type family ConstructorIsRecord (con :: Meta.ConstructorInfo) :: Constraint where
    ConstructorIsRecord (Meta.Record _ _) = ()
    ConstructorIsRecord (Meta.Infix _ _ _) = TypeError ('Text "Infix constructor are not supported")
    ConstructorIsRecord (Meta.Constructor _) = TypeError ('Text "All fields must have name to associate it with bin")

type IsRecord a xs con =
    ( Generic a
    , HasDatatypeInfo a
    , Code a ~ '[xs]
    , ConstructorInfos (DatatypeInfoOf a) ~ '[con]
    , ConstructorIsRecord con
    )

gToAsBins ::
    forall a xs con.
    ( IsRecord a xs con
    , All ToValue xs
    ) =>
    a ->
    [(BinName, Value)]
gToAsBins r =
    let
        fields = unZ . unSOP . from $ r
        fieldsInfo = datatypeInfo (Proxy @a)
     in
        case fieldsInfo of
            ADT _ _ (Record _ fieldNames :* Nil) _ -> hcollapse $ czipWith_NP (Proxy @ToValue) mkBin fieldNames fields
            Newtype _ _ (Record _ name) -> hcollapse $ czipWith_NP (Proxy @ToValue) mkBin name fields
            _ -> error "try to serialize unsupported data type"
  where
    -- TODO: add constraint at type level that fields have length no more than 15
    mkBin :: forall t. (ToValue t) => FieldInfo t -> I t -> K (BinName, Value) t
    mkBin (FieldInfo name) (I x) = K (fromJust $ mkBinName $ BS.pack name, toValue x)

gFromAsBins ::
    forall a xs con.
    ( IsRecord a xs con
    , All FromValue xs
    ) =>
    [(BinName, Value)] ->
    Maybe a
gFromAsBins bins =
    let
        mp = M.fromList $ (\(k, v) -> (k.bs, v)) <$> bins
        lookup name = mp M.!? BS.pack name

        extractField :: forall t. (FromValue t) => FieldInfo t -> Maybe t
        extractField (FieldInfo name) = do
            value <- lookup name
            fromValue value
     in
        case datatypeInfo (Proxy @a) of
            ADT _ _ (Record _ fieldNames :* Nil) _ -> do
                fields <- sequence_NP $ cmap_NP (Proxy @FromValue) extractField fieldNames
                pure $ to (SOP (Z fields))
            Newtype _ _ (Record _ name) -> do
                fields <- sequence_NP $ cmap_NP (Proxy @FromValue) extractField name
                pure $ to (SOP (Z fields))
            _ -> error "try to serialize unsupported data type"

instance forall a xs con. (IsRecord a xs con, All ToValue xs) => ToAsBins (Generically a) where
    toAsBins (Generically x) = gToAsBins x

instance forall a xs con. (IsRecord a xs con, All FromValue xs) => FromAsBins (Generically a) where
    fromAsBins = fmap Generically . gFromAsBins
