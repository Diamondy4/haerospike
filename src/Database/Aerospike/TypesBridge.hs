{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Aerospike.TypesBridge where

import Control.Monad (forM, forM_)
import Control.Monad.Cont
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Foreign qualified as TF
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Data.Word (Word8)
import Database.Aerospike.Internal
import Database.Aerospike.Internal.Raw
import Database.Aerospike.Key
import Database.Aerospike.Record
import Database.Aerospike.Value
import Foreign
import Foreign.C
import GHC.IO.Handle.Text (memcpy)
import Language.C.Inline qualified as C

C.context (C.baseCtx <> C.bsCtx <> C.fptrCtx <> asCtx <> C.vecCtx <> C.funCtx)
C.include "<aerospike/aerospike.h>"
C.include "<aerospike/aerospike_key.h>"
C.include "<aerospike/as_key.h>"
C.include "<aerospike/as_bin.h>"
C.include "<aerospike/as_nil.h>"
C.include "<aerospike/as_orderedmap.h>"
C.include "<aerospike/as_arraylist.h>"
C.include "<string.h>"

-- FIXME: should $bs-cstr be copied?
initKey :: AsKey -> Key -> ContT r IO ()
initKey ptr key = do
    cNamespace <- ContT $ BS.useAsCString key.namespace
    cSet <- ContT $ BS.useAsCString key.set

    case key.pKey of
        KInteger k ->
            lift
                [C.block| void {
                    as_key_init_int64(
                        $fptr-ptr:(as_key* ptr), 
                        $(char* cNamespace), 
                        $(char* cSet), 
                        $(int64_t k)
                    );
            }|]
        KString k -> do
            bsK <- ContT $ BS.useAsCString $ encodeUtf8 k
            lift
                [C.block| void {
                    as_key_init($fptr-ptr:(as_key* ptr), $(char* cNamespace), $(char* cSet), $(char* bsK));
                }|]
        KBytes k -> do
            (keyPtr, keyLen) <- ContT $ BS.useAsCStringLen k
            let cKeyLen = toEnum @Word32 keyLen
            lift
                [C.block| void {
                    as_key_init_raw(
                        $fptr-ptr:(as_key* ptr), 
                        $(char* cNamespace), 
                        $(char* cSet), 
                        $(char* keyPtr), 
                        $(uint32_t cKeyLen));
                }|]

newKey :: Key -> ContT r IO AsKey
newKey key = do
    asKeyPtr <- lift [C.block| as_key* { return (as_key*)malloc(sizeof(as_key)); }|]
    finalizer <- lift [C.exp|void (*free)(as_key*) { (void (*)(as_key*))&free }|]
    asKey <- lift $ AsKey <$> newForeignPtr finalizer asKeyPtr
    initKey asKey key
    pure asKey

-- TODO: probably should signal error if parsing of inner value in collection failed
-- TODO: should it return Maybe? hard to imagine cases where it can be needed
-- (aside from bugs in aerospike-c-client itself )
parseBinValue :: AsVal -> IO (Maybe Value)
parseBinValue ptrVal = do
    res <- newIORef @(Maybe Value) Nothing

    let setNil :: IO ()
        setNil = writeIORef res (Just VNil)

    let setBool :: CBool -> IO ()
        setBool v =
            if v > 0
                then writeIORef res (Just $ VBoolean True)
                else writeIORef res (Just $ VBoolean False)

    let setInt :: Int64 -> IO ()
        setInt v = writeIORef res (Just $ VInteger $ fromIntegral v)

    let setString :: CString -> CInt -> IO ()
        setString ptr len = do
            text <- TF.peekCStringLen (ptr, fromIntegral len)
            writeIORef res $ Just $ VString text

    let setBytes :: Ptr CChar -> CInt -> IO ()
        setBytes ptr len = do
            bytes <- BS.packCStringLen (ptr, fromIntegral len)
            writeIORef res $ Just $ VBytes bytes

    listRes <- newIORef @[Value] []
    let listCallback :: Ptr AsVal -> Ptr () -> IO CBool
        listCallback valPtr _ = do
            asVal <- AsVal <$> newForeignPtr_ valPtr
            val <- parseBinValue asVal
            forM_ val $ \val -> do
                modifyIORef listRes (val :)
            pure $ CBool 1

    let setList :: IO ()
        setList = do
            list <- readIORef listRes
            let vec = V.fromList $ reverse list
            writeIORef res (Just $ VList vec)

    mapRes <- newIORef @[(Value, Value)] []
    let mapCallback :: Ptr AsVal -> Ptr AsVal -> Ptr () -> IO CBool
        mapCallback keyPtr valPtr _ = do
            asKey <- AsVal <$> newForeignPtr_ keyPtr
            asVal <- AsVal <$> newForeignPtr_ valPtr
            key <- parseBinValue asKey
            val <- parseBinValue asVal
            forM_ ((,) <$> key <*> val) $ \p -> do
                modifyIORef mapRes (p :)
            pure $ CBool 1

    let setMap :: IO ()
        setMap = do
            map <- readIORef mapRes
            writeIORef res (Just $ VMap $ Map.fromList map)

    status <-
        [C.block| void {
        as_val* val = $fptr-ptr:(as_val* ptrVal);

        switch (as_val_type(val)) {
            case AS_NIL:
                $fun:(void (*setNil)())();
                break;

            case AS_BOOLEAN:
                as_boolean* bool_val = (as_boolean*)val;
                $fun:(void (*setBool)(bool))(bool_val->value);
                break;

            case AS_INTEGER:
                as_integer* int_val = (as_integer*)val;
                $fun:(void (*setInt)(int64_t))(int_val->value);
                break;

            case AS_STRING:
                as_string* str_val = (as_string*)val;
                $fun:(void (*setString)(char*, int))(as_string_get(str_val), as_string_len(str_val));
                break;

            case AS_BYTES:
                as_bytes* bytes_val = (as_bytes*)val;
                $fun:(void (*setBytes)(char*, int))((char*)as_bytes_get(bytes_val), as_bytes_size(bytes_val));
                break;

            case AS_LIST:
                as_list_foreach((as_list*)val, $fun:(bool (*listCallback)(as_val*, void*)), NULL);
                $fun:(void (*setList)())();
                break;

            case AS_MAP:
                as_map_foreach((as_map*)val, $fun:(bool (*mapCallback)(const as_val*, const as_val*, void*)), NULL);
                $fun:(void (*setMap)())();
                break;
        }
    }|]

    readIORef res

-- TODO: should allocate objects at C side to not think about lifetimes between FFI boundaries?
-- FIXME: actually broken, because want to use in code like this:
-- ```
-- val <- createVal hVal
-- insertInRecord rec val
-- ```
-- but it actually forgetting about pointers relations at this moment and can destroy
-- `val` because it's "unused"
-- TODO: Is it correct to leave destroy operations to outer structure?
-- i.e. it's seems like as_list call destroy operation at each element and as_record
-- destroys its associated resources (probably including keys and bin values)
createVal :: Value -> ContT r IO (Ptr AsVal)
createVal = \case
    VNil -> lift [C.exp| as_val* { (as_val*)&as_nil }|]
    VBoolean bool -> lift $ do
        let cBool = if bool then CBool 1 else CBool 0
        [C.block| as_val* { return (as_val*)as_boolean_new($(bool cBool)); }|]
    VInteger v -> lift [C.block| as_val* { return (as_val*)as_integer_new($(int64_t v)); }|]
    VString text -> do
        bs <- ContT $ BS.useAsCString $ encodeUtf8 text
        lift [C.block| as_val* { return (as_val*)as_string_new($(char* bs), false); } |]
    VList vec -> do
        let len = toEnum @Word32 $ V.length vec
        asList <- lift [C.block| as_val* { return (as_val*)as_arraylist_new($(uint32_t len), 1); }|]

        V.forM_ vec $ \x -> do
            val <- createVal x
            lift
                [C.block| void {
                    as_list_append((as_list*)$(as_val* asList), $(as_val* val));
                }|]

        pure asList
    VMap map -> do
        let len = toEnum @Word32 $ Map.size map
        asMap <- lift [C.block| as_val* { return (as_val*)as_orderedmap_new($(uint32_t len)); }|]

        forM_ (Map.assocs map) $ \(k, v) -> do
            cK <- createVal k
            cV <- createVal v
            lift
                [C.block| void {
                    as_map_set((as_map*)$(as_val* asMap), $(as_val* cK), $(as_val* cV));
                }|]

        pure asMap
    VBytes bytes -> do
        (bytesPtr, bytesLen) <- ContT $ BS.useAsCStringLen bytes
        let cBytesLen = toEnum @Word32 bytesLen
        lift
            [C.block| as_val* { 
            return (as_val*)as_bytes_new_wrap(
                $(char* bytesPtr), 
                $(uint32_t cBytesLen), 
                false); 
            }|]

byteStringFromParts :: Ptr CString -> Ptr CInt -> IO BS.ByteString
byteStringFromParts strPtr lenPtr = do
    str <- peek strPtr
    len <- peek lenPtr
    BS.packCStringLen (str, fromIntegral len)

parseRecord :: forall a. (FromAsBins a) => Ptr AsRecord -> IO (Maybe (Record a))
parseRecord recordPtr = do
    binsCount <-
        [C.exp| int { $(as_record* recordPtr)->bins.size }|]

    bins <-
        alloca @CString $ \binName ->
            alloca @CInt $ \binNameLen -> do
                forM [0 .. binsCount - 1] $ \binIx -> do
                    val <-
                        [C.block| as_val* {
                    as_record* r = $(as_record* recordPtr); 
                    as_bin* bin = r->bins.entries + $(int binIx);
                    char* bin_name = as_bin_get_name(bin);
                    *$(char** binName) = bin_name;
                    *$(int* binNameLen) = strlen(bin_name);
                    return (as_val*)as_record_get(r, bin_name);
                    }|]

                    bsBinName <- byteStringFromParts binName binNameLen
                    binVal <- AsVal <$> newForeignPtr_ val
                    binValue <- parseBinValue binVal
                    pure (bsBinName, binValue)

    gen <- [C.exp| uint16_t { $(as_record* recordPtr)->gen }|]
    ttl <- [C.exp| uint32_t { $(as_record* recordPtr)->ttl }|]

    pure $ do
        bins' <- forM bins (\(k, v) -> (,) <$> Just k <*> v)
        bins'' <- fromAsBins bins'

        pure $
            MkRecord
                { gen = gen
                , ttl = ttl
                , bins = bins''
                }
