{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Aerospike.Operations where

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
    as_key_init_str(&key, $bs-cstr:ns, $bs-cstr:set, $bs-cstr:key);

    as_record rec;
    as_record_inita(&rec, 1);
     
    rec.ttl = $(int ttlSec);
    
    as_record_set_str(&rec, $bs-cstr:binName, $bs-cstr:binStrData);
    
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
    IO (Either AerospikeError (Maybe ByteString))
getStrBinUpdateTTL as ns set key bin (fromIntegral -> ttlSec) = alloca @AerospikeError $ \errTmp -> alloca @CString $ \strTmp -> do
    status <-
        toEnum @AerospikeStatus . fromIntegral
            <$> [C.block| int {
    as_key key;
    as_key_init_str(&key, $bs-cstr:ns, $bs-cstr:set, $bs-cstr:key);

    as_record rec;
    as_record *recPtr = as_record_inita(&rec, 1);
    
    //const char* bins[] = { $bs-cstr:bin, NULL };
    //as_status status = aerospike_key_select($fptr-ptr:(aerospike* as), $(as_error* errTmp), NULL, &key, bins, &recPtr);

    as_operations ops;
    as_operations_inita(&ops, 2);

    ops.ttl = $(int ttlSec);

    as_operations_add_read(&ops, $bs-cstr:bin);
    as_operations_add_touch(&ops);

    as_status status = aerospike_key_operate($fptr-ptr:(aerospike* as), $(as_error* errTmp), NULL, &key, &ops, &recPtr);

    if(status != AEROSPIKE_OK){
        return status;
    }

    as_bin_value* value = as_record_get(&rec, $bs-cstr:bin);
    if (value == NULL) {
        return AEROSPIKE_ERR_BIN_NOT_FOUND;
    }
    
    //char* sbin = as_record_get_str(&rec, $bs-cstr:bin); return to haskell with by pointer
    *$(char** strTmp) = as_record_get_str(&rec, $bs-cstr:bin);

    return status;
    //as_record_destroy(rec); is not needed - rec on stack
    }|]
    case status of
        AerospikeOk -> do
            str <- peek strTmp >>= BS.packCString
            return . Right . Just $ str
        AerospikeErrBinNotFound -> pure $ Right Nothing
        AerospikeErrRecordNotFound -> pure $ Right Nothing
        _ -> Left <$> peek errTmp