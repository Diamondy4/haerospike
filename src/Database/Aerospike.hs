{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Aerospike where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Proxy
import Database.Aerospike.Internal
import Database.Aerospike.Internal.Raw
import Foreign
import Foreign.C
import GHC.IO.Handle.Text (memcpy)
import qualified Language.C.Inline as C
import Text.Printf (printf)

C.context (C.baseCtx <> C.bsCtx <> C.fptrCtx <> asCtx)
C.include "<aerospike/aerospike.h>"
C.include "<aerospike/aerospike_key.h>"

--aerospikeDestroy = [C.exp|void(*aerospike_destroy_t)(*aerospike){&aerospike_destroy}|]


createAerospikeClient :: ByteString -> Int -> IO Aerospike
createAerospikeClient address port' = do
    let port = fromIntegral port'
    as <-
        [C.block| aerospike* {
    as_config config;
    as_config_init(&config);
    as_config_add_host(&config, $bs-ptr:address, $(int port));
    aerospike* as = aerospike_new(&config);
    return as;
    }|]
    Aerospike <$> newForeignPtr_   as

connectAerospikeClient :: Aerospike -> IO (Either AerospikeError ())
connectAerospikeClient (Aerospike as) = alloca @AerospikeError $ \errTmp -> do
    status <- toEnum @AerospikeStatus . fromIntegral <$> [C.exp| int { aerospike_connect($fptr-ptr:(aerospike* as), $(as_error* errTmp)) }|]
    case status of
        AerospikeOk -> return . Right $ ()
        _ -> Left <$> peek errTmp

setStrBin ::
    Aerospike ->
    ByteString ->
    ByteString ->
    ByteString ->
    ByteString ->
    ByteString ->
    Int ->
    IO (Either AerospikeError ())
setStrBin as ns set key binName binStrData (fromIntegral -> ttlSec) = alloca @AerospikeError $ \errTmp -> do
    status <-
        toEnum @AerospikeStatus . fromIntegral
            <$> [C.block| int {
    as_key key;
    as_key_init_str(&key, $bs-ptr:ns, $bs-ptr:set, $bs-ptr:key);

    as_record rec;
    as_record_inita(&rec, 1);
     
    rec.ttl = $(int ttlSec);
    
    as_record_set_str(&rec, $bs-ptr:binName, $bs-ptr:binStrData);
    
    return aerospike_key_put($fptr-ptr:(aerospike* as), $(as_error* errTmp), NULL, &key, &rec);
    }|]
    case status of
        AerospikeOk -> return . Right $ ()
        _ -> Left <$> peek errTmp

getStrBinUpdateTTL ::
    Aerospike ->
    ByteString ->
    ByteString ->
    ByteString ->
    ByteString ->
    Int ->
    IO (Either AerospikeError ByteString)
getStrBinUpdateTTL as ns set key bin (fromIntegral -> ttlSec) = alloca @AerospikeError $ \errTmp -> alloca @CString $ \strTmp -> do
    status <-
        toEnum @AerospikeStatus . fromIntegral
            <$> [C.block| int {
    as_key key;
    as_key_init_str(&key, $bs-ptr:ns, $bs-ptr:set, $bs-ptr:key);

    as_record rec;
    as_record *recPtr = as_record_inita(&rec, 1);
    
    //const char* bins[] = { $bs-ptr:bin, NULL };
    //as_status status = aerospike_key_select($fptr-ptr:(aerospike* as), $(as_error* errTmp), NULL, &key, bins, &recPtr);

    as_operations ops;
    as_operations_inita(&ops, 2);

    ops.ttl = $(int ttlSec);

    as_operations_add_read(&ops, $bs-ptr:bin);
    as_operations_add_touch(&ops);

    as_status status = aerospike_key_operate($fptr-ptr:(aerospike* as), $(as_error* errTmp), NULL, &key, &ops, &recPtr);

    if(status != AEROSPIKE_OK){
        return status;
    }

    //char* sbin = as_record_get_str(&rec, $bs-ptr:bin);
    *$(char** strTmp) = as_record_get_str(&rec, $bs-ptr:bin);

    return status;
    //as_record_destroy(rec); is not needed - rec on stack
    }|]
    case status of
        AerospikeOk -> do
            str <- peek strTmp >>= BS.packCString
            return . Right $ str
        _ -> Left <$> peek errTmp