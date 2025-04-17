{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text (Text)
import Database.Aerospike
import Database.Aerospike.Internal.Raw
import Database.Aerospike.Key
import Database.Aerospike.Operations
import Database.Aerospike.Operator
import Database.Aerospike.Value

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

setTest :: Aerospike -> ByteString -> ByteString -> IO ()
setTest as ns set = do
    let binA = "binA"
    let binB = "binB"
    let key = "test_key"
    let key1 = MkKey ns set (KBytes key)

    res <- keyPut as key1 [(binA, VString "binAValueModified"), (binB, VString "binBValueModified")]
    print res

batchTest :: Aerospike -> ByteString -> ByteString -> IO ()
batchTest as ns set = do
    let binA = "binA"
    let binB = "binB"
    let binAValue = "binAValue"
    let binBValue = "binBValue"
    let key = "test_key"

    val <- setBinBytesToString as ns set key binA binAValue 120
    print val

    val <- setBinBytesToString as ns set key binB binBValue 120
    print val

    let key1 = MkKey ns set (KBytes key)
    let key2 = MkKey ns set (KString "key2")
    let key3 = MkKey ns set (KString "key3")

    vals <- keyBatchedGet as [key1, key2, key3]
    print vals

    res <- keyPut as key1 [(binA, VString "binAValueModified"), (binB, VString "binBValueModified")]
    print res

    vals <- keyBatchedGet as [key1]
    print vals

operateTest :: Aerospike -> ByteString -> ByteString -> IO ()
operateTest as ns set = do
    let opKey = MkKey ns set (KString "opKey")
    let binA = "binA"
    let binB = "binB"
    let binC = "binC"

    res <- keyPut as opKey [(binA, VString "A"), (binB, VInteger 10)]
    print res

    vals <- keyBatchedGet as [opKey]
    print vals

    res <-
        keyOperate
            as
            opKey
            [ Write $ WriteOp{binName = binA, value = VString "operate A"}
            , Write $ WriteOp{binName = binB, value = VInteger 42}
            , Modify binC (MPut $ Map.fromList [(VString "field1", VInteger 42), (VString "field2", VInteger 4)])
            , SetTTL $ ManualSecs 100
            , Read $ ReadOne binA
            , Read $ ReadOne binC
            ]

    print res

    vals <- keyBatchedGet as [opKey]
    print vals

main :: IO ()
main = do
    let ip = "127.0.0.1" :: ByteString
        ns = "test"
        set = "test_set"

    as <- createAerospikeClient ip 3000 100
    setLogCallbackAerospike (\a b c d e -> print (a, b, c, d, e) >> pure True)
    setLogLevelAerospike AsLogLevelTrace
    conRes <- connectAerospikeClient as
    print conRes

    simpleTest as ns set

    -- Run many times to ensure that it will not crash by memory corruption
    forM_ [0 .. 100] $ \_ -> do
        setTest as ns set
        batchTest as ns set
        operateTest as ns set

    print "done"
