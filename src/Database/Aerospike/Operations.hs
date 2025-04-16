{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Aerospike.Operations where

import Control.Monad (forM, forM_)
import Control.Monad.Cont
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Foreign qualified as TF
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Database.Aerospike.Internal
import Database.Aerospike.Internal.Raw
import Database.Aerospike.Key
import Database.Aerospike.Operator
import Database.Aerospike.TypesBridge
import Database.Aerospike.Value
import Foreign
import Foreign.C
import GHC.IO.Handle.Text (memcpy)
import Language.C.Inline qualified as C

C.context (C.baseCtx <> C.bsCtx <> C.fptrCtx <> asCtx <> C.vecCtx)
C.include "<aerospike/aerospike.h>"
C.include "<aerospike/aerospike_key.h>"
C.include "<aerospike/aerospike_batch.h>"
C.include "<aerospike/as_vector.h>"
C.include "<aerospike/as_operations.h>"
C.include "<aerospike/as_record.h>"

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

byteStringFromParts :: Ptr CString -> Ptr CInt -> IO BS.ByteString
byteStringFromParts strPtr lenPtr = do
    str <- peek strPtr
    len <- peek lenPtr
    BS.packCStringLen (str, fromIntegral len)

mkAsBatchRecords :: Int -> IO AsBatchRecords
mkAsBatchRecords keys = do
    let ckeys = CInt $ toEnum keys
    records <- [C.block| as_batch_records* { return as_batch_records_create($(int ckeys)); }|]
    finalizer <- [C.exp|void (*as_batch_records_destroy)(as_batch_records*) { &as_batch_records_destroy }|]
    AsBatchRecords <$> newForeignPtr finalizer records

-- TODO: how does actually $bs-cstr works? Should it be copied again to avoid GC
-- free it early?
-- namespace -> set -> [key] -> [[(bin_name, bin_value)]]
getBatchedKeysAllBinsValues ::
    Aerospike ->
    [Key] ->
    IO (Either AerospikeError [Maybe [(ByteString, Maybe Value)]])
getBatchedKeysAllBinsValues as keys = evalContT $ do
    let keysLen = length keys
    let ckeysLen = CInt $ toEnum keysLen

    errTmp <- ContT $ alloca @AerospikeError
    records <- lift $ mkAsBatchRecords keysLen

    forM_ keys $ \key -> do
        asKeyPtr <-
            lift
                [C.block| as_key* {
                as_batch_read_record* record = as_batch_read_reserve($fptr-ptr:(as_batch_records* records));
                record->read_all_bins = true;
                return &record->key;
                }|]

        asKey <- lift $ AsKey <$> newForeignPtr_ asKeyPtr
        initKey asKey key

    status <-
        lift $
            statusFromCSide
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
            binName <- ContT $ alloca @CString
            binNameLen <- ContT $ alloca @CInt
            -- TODO: check records.list.size instead of rely on key_len?
            res <- forM [0 .. ckeysLen - 1] $ \i -> lift $ do
                status <-
                    statusFromCSide
                        <$> [C.block| int {
                    as_vector* list = &$fptr-ptr:(as_batch_records* records)->list;
                    as_batch_read_record* r = as_vector_get(list, $(int i));
                    return r->result;         
                    }|]

                case status of
                    AerospikeOk ->
                        Just <$> do
                            binsCount <-
                                [C.block| int {
                                as_vector* list = &$fptr-ptr:(as_batch_records* records)->list;
                                as_batch_read_record* r = as_vector_get(list, $(int i));
                                return r->record.bins.size;   
                                }|]

                            forM [0 .. binsCount - 1] $ \binIx -> do
                                val <-
                                    [C.block| as_val* {
                                    as_vector* list = &$fptr-ptr:(as_batch_records* records)->list;
                                    as_batch_read_record* r = as_vector_get(list, $(int i));
                                    as_bin* bin = r->record.bins.entries + $(int binIx);
                                    
                                    char* bin_name = as_bin_get_name(bin);
                                    *$(char** binName) = bin_name;
                                    *$(int* binNameLen) = strlen(bin_name);

                                    return (as_val*)as_record_get(&r->record, bin_name);
                                    }|]

                                bsBinName <- byteStringFromParts binName binNameLen
                                binVal <- AsVal <$> newForeignPtr_ val
                                binValue <- parseBinValue binVal
                                pure (bsBinName, binValue)
                    _ -> pure Nothing
            return . Right $ res
        _ -> do
            err <- lift $ peek errTmp
            pure $ Left err

statusFromCSide :: CInt -> AerospikeStatus
statusFromCSide = toEnum @AerospikeStatus . fromIntegral

setKey ::
    Aerospike ->
    Key ->
    [(ByteString, Value)] ->
    IO (Either AerospikeError ())
setKey as key bins = evalContT $ do
    let binsLen = CInt $ toEnum $ length bins
    asRecordPtr <- lift [C.block| as_record* { return as_record_new($(int binsLen)); }|]
    finalizer <- lift [C.exp|void (*as_record_destroy)(as_record*) { &as_record_destroy }|]
    asRecord <- lift $ AsRecord <$> newForeignPtr finalizer asRecordPtr

    forM_ bins $ \(binName, binValue) -> do
        val <- createVal binValue
        cBinName <- ContT $ BS.useAsCString binName
        lift
            [C.block| void {
                as_record_set($fptr-ptr:(as_record* asRecord), $(char* cBinName), (as_bin_value*)$(as_val* val));
            }|]

    asKey <- newKey key
    errTmp <- ContT $ alloca @AerospikeError

    status <-
        lift $
            statusFromCSide
                <$> [C.block| int {
                return aerospike_key_put(
                    $fptr-ptr:(aerospike* as),
                    $(as_error* errTmp),
                    NULL,
                    $fptr-ptr:(as_key* asKey),
                    $fptr-ptr:(as_record* asRecord)
                );
            }|]

    case status of
        AerospikeOk -> pure $ Right ()
        _ -> do
            err <- lift $ peek errTmp
            pure $ Left err

-- TODO: get read fields
keyOperate ::
    Aerospike ->
    Key ->
    [Operator] ->
    IO (Either AerospikeError ())
keyOperate as key ops = evalContT $ do
    let opsLen = CInt $ toEnum $ length ops
    asKey <- newKey key
    asOperationsPtr <- lift [C.block| as_operations* { return as_operations_new($(int opsLen)); }|]
    finalizer <- lift [C.exp|void (*as_operations_destroy)(as_operations*) { &as_operations_destroy }|]
    asOperations <- lift $ AsOperations <$> newForeignPtr finalizer asOperationsPtr

    forM_ ops $ \case
        Read op -> do
            bin <- ContT $ BS.useAsCString op.binName
            lift
                [C.block| void {
                as_operations_add_read($fptr-ptr:(as_operations* asOperations), $(char* bin));
            }|]
        Write op -> do
            bin <- ContT $ BS.useAsCString op.binName
            asVal <- createVal op.value
            lift
                [C.block| void {
                as_operations_add_write(
                    $fptr-ptr:(as_operations* asOperations), 
                    $(char* bin), 
                    (as_bin_value*)$(as_val* asVal)
                );
            }|]
        Touch ->
            lift
                [C.block| void {
                as_operations_add_touch($fptr-ptr:(as_operations* asOperations));
            }|]
        Modify -> error "todo"
        SetTTL ttl -> lift $ do
            let ttlVal = case ttl of
                    DefaultTTL -> [C.pure| int { AS_RECORD_DEFAULT_TTL } |]
                    NoExpireTTL -> [C.pure| int { AS_RECORD_NO_EXPIRE_TTL } |]
                    NoChangeTTL -> [C.pure| int {AS_RECORD_NO_CHANGE_TTL }|]
                    ClientDefaultTTL -> [C.pure| int {AS_RECORD_CLIENT_DEFAULT_TTL }|]
                    ManualSecs secs -> CInt $ fromIntegral secs

            [C.block| void {
                $fptr-ptr:(as_operations* asOperations)->ttl = $(int ttlVal);
            }|]
        Delete ->
            lift
                [C.block| void {
                as_operations_add_delete($fptr-ptr:(as_operations* asOperations));
            }|]

    errTmp <- ContT $ alloca @AerospikeError
    status <-
        lift $
            statusFromCSide
                <$> [C.block| int {
            as_record* r = NULL;

            return aerospike_key_operate(
                $fptr-ptr:(aerospike* as),
                $(as_error* errTmp),
                NULL,
                $fptr-ptr:(as_key* asKey),
                $fptr-ptr:(as_operations* asOperations),
                &r
            );
        }|]

    case status of
        AerospikeOk -> pure $ Right ()
        _ -> lift $ Left <$> peek errTmp
