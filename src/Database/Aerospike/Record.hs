{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Database.Aerospike.Record where

import Data.ByteString qualified as BS hiding (pack)
import Data.ByteString.Char8 qualified as BS
import Data.Map qualified as M
import Data.Word (Word16, Word32)
import Database.Aerospike.Value (
    FromValue (..),
    ToValue (..),
    Value,
 )

import Data.Kind (Constraint)
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

data Record a = MkRecord
    { gen :: Word16
    -- ^ Generation of the record. Updated each time modification performed on record.
    , ttl :: Word32
    -- ^ The time-to-live (expiration) of the record in seconds.
    , bins :: a
    }
    deriving stock (Show, Eq, Ord)

class FromAsBins a where
    fromAsBins :: [(BS.ByteString, Value)] -> Maybe a
    default fromAsBins ::
        ( IsRecord a xs con
        , All FromValue xs
        ) =>
        [(BS.ByteString, Value)] ->
        Maybe a
    fromAsBins = gFromAsBins

class ToAsBins a where
    toAsBins :: a -> [(BS.ByteString, Value)]
    default toAsBins ::
        ( IsRecord a xs con
        , All ToValue xs
        ) =>
        a ->
        [(BS.ByteString, Value)]
    toAsBins = gToAsBins

instance FromAsBins [(BS.ByteString, Value)] where
    fromAsBins = Just

instance ToAsBins [(BS.ByteString, Value)] where
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
    [(BS.ByteString, Value)]
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
    mkBin :: forall t. (ToValue t) => FieldInfo t -> I t -> K (BS.ByteString, Value) t
    mkBin (FieldInfo name) (I x) = K (BS.pack name, toValue x)

gFromAsBins ::
    forall a xs con.
    ( IsRecord a xs con
    , All FromValue xs
    ) =>
    [(BS.ByteString, Value)] ->
    Maybe a
gFromAsBins bins =
    let
        mp = M.fromList bins
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
