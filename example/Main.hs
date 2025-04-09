{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Database.Aerospike
import Database.Aerospike.Internal.Raw
import Database.Aerospike.Operations

main :: IO ()
main = do
    let ip = "127.0.0.1" :: ByteString
        ns = "test"
        set = "test_set"
        key = "test_key"
        binName = "test_bin"
        binStrData = "bin_str_data\0afterNull"
    as <- createAerospikeClient ip 3000 100
    setLogCallbackAerospike (\a b c d e -> print (a, b, c, d, e) >> pure True)
    setLogLevelAerospike AsLogLevelTrace
    conRes <- connectAerospikeClient as
    print conRes
    val <- setBinBytesToString as ns set key binName binStrData 120
    print val
    val <- getBinBytesToStringUpdateTTL as ns set key binName 120
    print val
    val <- getBinBytesToStringUpdateTTL as ns set key binName 120
    print val

    let binA = "binA"
    let binB = "binB"
    let binAValue = "binAValue"
    let binBValue = "binBValue"

    val <- setBinBytesToString as ns set key binA binAValue 120
    print val

    val <- setBinBytesToString as ns set key binB binBValue 120
    print val

    vals <- getBatchedKeysAllBins as ns set [key]
    print vals

    val <- getBinBytesToStringUpdateTTL as ns set key binName 120
    print val

    print "done"
