{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Database.Aerospike
import Database.Aerospike.Operations
import Database.Aerospike.Internal.Raw

main :: IO ()
main = do
    let ip = "127.0.0.1" :: ByteString
        ns = "test"
        set = "test_set"
        key = "test_key"
        binName = "test_bin"
        binStrData = "bin_str_data"
    as <- createAerospikeClient ip 3000
    setLogCallbackAerospike (\a b c d e -> print (a, b, c, d, e) >> pure True)
    setLogLevelAerospike AsLogLevelTrace
    conRes <- connectAerospikeClient as
    print conRes
    val <- setStrBin as ns set key binName binStrData 120
    print val
    val <- getStrBinUpdateTTL as ns set key binName 120
    print val

    print "done"