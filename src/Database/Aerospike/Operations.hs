{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Aerospike.Operations where

import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Database.Aerospike.Internal
import Database.Aerospike.Internal.Raw
import Foreign
import Foreign.C
import GHC.IO.Handle.Text (memcpy)
import qualified Language.C.Inline as C

C.context (C.baseCtx <> C.bsCtx <> C.fptrCtx <> asCtx <> C.vecCtx)
C.include "<aerospike/aerospike.h>"
C.include "<aerospike/aerospike_key.h>"
C.include "<aerospike/aerospike_batch.h>"

setBinBytesToString ::
    Aerospike ->
    ByteString ->
    ByteString ->
    ByteString ->
    ByteString ->
    Text ->
    Int ->
    IO (Either AerospikeError ())
setBinBytesToString as ns set key binName binStrData (fromIntegral -> ttlSec) =
    alloca @AerospikeError $ \errTmp ->
        TF.withCStringLen binStrData $ \(cBinStrData, fromIntegral -> cBinStrDataLen) -> do
            status <-
                toEnum @AerospikeStatus . fromIntegral
                    <$> [C.block| int {
    as_key key;
    as_key_init_raw(&key, $bs-cstr:ns, $bs-cstr:set, $bs-ptr:key, $bs-len:key);

    as_record rec;
    as_record_inita(&rec, 1);
     
    rec.ttl = $(int ttlSec);

    as_string str;
    as_string_init_wlen(&str, $(char* cBinStrData), $(int cBinStrDataLen), false);
    
    as_record_set_string(&rec, $bs-cstr:binName, &str);
    
    return aerospike_key_put($fptr-ptr:(aerospike* as), $(as_error* errTmp), NULL, &key, &rec);
    }|]
            case status of
                AerospikeOk -> return . Right $ ()
                _ -> Left <$> peek errTmp

getBinBytesToStringUpdateTTL ::
    Aerospike ->
    ByteString ->
    ByteString ->
    ByteString ->
    ByteString ->
    Int ->
    IO (Either AerospikeError (Maybe Text))
getBinBytesToStringUpdateTTL as ns set key bin (fromIntegral -> ttlSec) =
    alloca @AerospikeError $ \errTmp ->
        alloca @CString $ \strTmp ->
            alloca @CInt $ \strLenTmp -> do
                status <-
                    toEnum @AerospikeStatus . fromIntegral
                        <$> [C.block| int {
    as_key key;
    as_key_init_raw(&key, $bs-cstr:ns, $bs-cstr:set, $bs-ptr:key, $bs-len:key);

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

    as_string * value = as_record_get_string(&rec, $bs-cstr:bin);
    if (value == NULL) {
        return AEROSPIKE_ERR_BIN_NOT_FOUND;
    }
    
    //char* sbin = as_record_get_str(&rec, $bs-cstr:bin); return to haskell with by pointer
    //*$(char** strTmp) = as_record_get_string(&rec, $bs-cstr:bin);

    *$(char** strTmp) = as_string_get(value);
    *$(int* strLenTmp) = as_string_len(value);

    return status;
    //as_record_destroy(rec); is not needed - rec on stack
    }|]
                case status of
                    AerospikeOk -> do
                        str <- peek strTmp
                        len <- peek strLenTmp
                        txt <- TF.peekCStringLen (str, fromIntegral len)
                        return . Right . Just $ txt
                    AerospikeErrBinNotFound -> pure $ Right Nothing
                    AerospikeErrRecordNotFound -> pure $ Right Nothing
                    _ -> Left <$> peek errTmp

-- namespace -> set -> [key] -> [[(bin_name, bin_value)]]
getBatchedKeysAllBins ::
    Aerospike ->
    ByteString ->
    ByteString ->
    [ByteString] ->
    IO (Either AerospikeError [[(ByteString, ByteString)]])
getBatchedKeysAllBins as ns set keys = do
    let keysLen = CInt $ toEnum $ length keys
    alloca @AerospikeError $ \errTmp -> do
        records <- mkAsBatchRecords

        _ <-
            [C.block| void {
            as_batch_records_inita($fptr-ptr:(as_batch_records* records), $(int keysLen));
            }|]

        forM_ keys $ \key -> do
            [C.block| void {
            as_batch_read_record* record = as_batch_read_reserve($fptr-ptr:(as_batch_records* records));
            as_key_init(&record->key, $bs-cstr:ns, $bs-cstr:set, $bs-cstr:key);
            record->read_all_bins = true;
            }|]

        status <-
            toEnum @AerospikeStatus . fromIntegral
                <$> [C.block| int { 
            return aerospike_batch_read(
                $fptr-ptr:(aerospike* as), 
                $(as_error* errTmp), 
                NULL, 
                $fptr-ptr:(as_batch_records* records)
            );
            }|]

        case status of
            AerospikeOk -> do
                alloca @CString $ \binValue ->
                    alloca @CInt $ \binValueLen ->
                        alloca @CString $ \binName ->
                            alloca @CInt $ \binNameLen -> do
                                -- TODO: check records.list.size instead of rely on key_len?
                                res <- forM [0 .. keysLen - 1] $ \i -> do
                                    binsCount <-
                                        [C.block| int {
                                        as_vector* list = &$fptr-ptr:(as_batch_records* records)->list;
                                        as_batch_read_record* r = as_vector_get(list, $(int i));
                                        return r->record.bins.size;   
                                    }|]

                                    forM [0 .. binsCount - 1] $ \binIx -> do
                                        [C.block| void {
                                            as_vector* list = &$fptr-ptr:(as_batch_records* records)->list;
                                            as_batch_read_record* r = as_vector_get(list, $(int i));
                                            as_bin* bin = r->record.bins.entries + $(int binIx);
                                            
                                            char* bin_name = as_bin_get_name(bin);
                                            *$(char** binName) = bin_name;
                                            *$(int* binNameLen) = strlen(bin_name);

                                            as_string* value = as_record_get_string(&r->record, bin_name);
                                            *$(char** binValue) = as_string_get(value);
                                            *$(int* binValueLen) = as_string_len(value);
                                        }|]

                                        bsBinName <- byteStringFromParts binName binNameLen
                                        bsBinValue <- byteStringFromParts binValue binValueLen
                                        pure (bsBinName, bsBinValue)

                                return . Right $ res
            _ -> do
                [C.block| void { as_batch_records_destroy($fptr-ptr:(as_batch_records* records)); }|]
                Left <$> peek errTmp

byteStringFromParts :: Ptr CString -> Ptr CInt -> IO BS.ByteString
byteStringFromParts strPtr lenPtr = do
    str <- peek strPtr
    len <- peek lenPtr
    BS.packCStringLen (str, fromIntegral len)

-- TODO: add finalizer
mkAsBatchRecords :: IO AsBatchRecords
mkAsBatchRecords = do
    records <- [C.block| as_batch_records* { return malloc(sizeof(as_batch_records)); }|]
    AsBatchRecords <$> newForeignPtr_ records
