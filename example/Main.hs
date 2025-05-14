{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Vector qualified as V
import Database.Aerospike
import Database.Aerospike.Internal.Raw
import Database.Aerospike.Key
import Database.Aerospike.Operations
import Database.Aerospike.Operator
import Database.Aerospike.Record (BinName, FromAsBins (fromAsBins), RawBins, Record (..), ToAsBins (toAsBins), binNameBS, mkBinName)
import Database.Aerospike.Value
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP

binA :: BinName
binA = fromJust $ mkBinName "binA"

binB :: BinName
binB = fromJust $ mkBinName "binB"

binC :: BinName
binC = fromJust $ mkBinName "binC"

simpleTest :: Aerospike -> ByteString -> ByteString -> IO ()
simpleTest as ns set = do
    let key = "test_key"
    let binName = "test_bin"
    let binStrData = "bin_str_data\0afterNull"

    val <- setBinBytesToString as ns set key binName binStrData 120
    print val
    val <- getBinBytesToStringUpdateTTL as ns set key binName 120
    print val
    val <- getBinBytesToStringUpdateTTL as ns set key binName 120
    print val

setTest :: Aerospike -> Namespace -> Set -> IO ()
setTest as ns set = do
    let key = "test_key"
    let key1 = MkKey ns set (KBytes key)

    res <- keyPut @RawBins as Nothing key1 [(binA, VString "binAValueModified"), (binB, VString "binBValueModified")]
    print res

batchTest :: Aerospike -> Namespace -> Set -> IO ()
batchTest as ns set = do
    let binAValue = "binAValue"
    let binBValue = "binBValue"
    let key = "test_key"

    val <- setBinBytesToString as (namespaceBS ns) (setBS set) key (binNameBS binA) binAValue 120
    print val

    val <- setBinBytesToString as (namespaceBS ns) (setBS set) key (binNameBS binB) binBValue 120
    print val

    let key1 = MkKey ns set (KBytes key)
    let key2 = MkKey ns set (KString "key2")
    let key3 = MkKey ns set (KString "key3")

    vals <- keyBatchedGet @RawBins as Nothing [key1, key2, key3]
    print vals

    res <- keyPut @RawBins as Nothing key1 [(binA, VString "binAValueModified"), (binB, VString "binBValueModified")]
    print res

    vals <- keyGet @RawBins as Nothing key1
    print vals

operateTest :: Aerospike -> Namespace -> Set -> IO ()
operateTest as ns set = do
    let opKey = MkKey ns set (KString "opKey")

    res <- keyPut as Nothing opKey [(binA, VString "A"), (binB, VInteger 10)]
    print res

    vals <- keyBatchedGet @RawBins as Nothing [opKey]
    print vals

    res <-
        keyOperate
            @RawBins
            as
            Nothing
            opKey
            [ Write $ WriteOp{binName = binA, value = VString "operate A"}
            , Write $ WriteOp{binName = binB, value = VInteger 42}
            , Modify binC (MPut $ Map.fromList [(MString "field1", VInteger 42), (MString "field2", VInteger 4)])
            , SetTTL $ ManualSecs 100
            , Read $ ReadOne binA
            , Read $ ReadOne binC
            ]

    print res

    vals <- keyBatchedGet @RawBins as Nothing [opKey]
    print vals

data Foo = Foo
    { binA :: V.Vector Int64
    , binB :: Int64
    , binC :: Text
    , binD :: ByteString
    }
    deriving stock (Show, GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToAsBins, FromAsBins) via (GHC.Generically Foo)

recordTest :: Aerospike -> Namespace -> Set -> IO ()
recordTest as ns set = do
    let opKey = MkKey ns set (KString "recordTestKey")
    let r = Foo (V.fromList [1, 2, 3]) 42 "fieldC" "fieldD"

    print (toAsBins r)
    res <- keyPut as Nothing opKey (toAsBins r)
    print res

    res <- keyGet @RawBins as Nothing opKey
    print res

    res <- keyGet @Foo as Nothing opKey
    print res

main :: IO ()
main = do
    let ip = "127.0.0.1" :: ByteString
        ns = fromJust $ mkNamespace "test"
        set = fromJust $ mkSet "test_set"

    as <- createAerospikeClient ip 3000 100
    setLogCallbackAerospike (\a b c d e -> print (a, b, c, d, e) >> pure True)
    setLogLevelAerospike AsLogLevelTrace
    conRes <- connectAerospikeClient as
    print conRes

    simpleTest as (namespaceBS ns) (setBS set)

    -- Run many times to ensure that it will not crash by memory corruption
    forM_ [0 .. 100] $ \_ -> do
        setTest as ns set
        batchTest as ns set
        operateTest as ns set

    print "record test:"
    recordTest as ns set

    print "done"
