{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Aerospike where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Proxy
import Database.Aerospike.Internal
import Database.Aerospike.Internal.Raw
import Foreign
import Foreign.C
import Foreign.Marshal.Utils qualified as FMU
import Language.C.Inline qualified as C

C.context (C.baseCtx <> C.bsCtx <> C.fptrCtx <> C.funCtx <> asCtx)
C.include "<aerospike/aerospike.h>"
C.include "<aerospike/aerospike_key.h>"
C.include "<aerospike/as_log.h>"

{-
Dirty hack to convert aerospike callback with variadic parameters to haskell callback with char array parameter.
Needed due to haskell incompatibility with variadic function FFI (even capi need fixed parameter count).
Haskell FunPtr to log function is stored inside global variable, wrapper C log function is used
to collect log to char array and call haskell log function.
-}
C.verbatim "bool (*as_log_hs_callback)(as_log_level level, const char *func, const char *file, uint32_t line, const char *msg);"

-- Wrapper for variadic function hack
C.verbatim
    "bool as_log_hs_callback_wrapper(\n\
    \    as_log_level level, const char * func, const char * file, uint32_t line,\n\
    \    const char * fmt, ...)\n\
    \{\n\
    \    char msg[1024];\n\
    \    va_list ap;\n\
    \    va_start(ap, fmt);\n\
    \    vsnprintf(msg, 1024, fmt, ap);\n\
    \    va_end(ap);\n\
    \    return as_log_hs_callback(level, func, file, line, msg);\n\
    \}"

createAerospikeClient :: ByteString -> Int -> Int -> IO Aerospike
createAerospikeClient address (fromIntegral -> port) (fromIntegral -> connCount) = do
    as <-
        [C.block| aerospike* {
    as_config config;
    as_config_init(&config);
    as_config_add_host(&config, $bs-cstr:address, $(int port));
    config.max_conns_per_node = $(int connCount);
    aerospike* as = aerospike_new(&config);
    return as;
    }|]
    asFinalizer <- aerospikeDestroy
    Aerospike <$> newForeignPtr asFinalizer as

connectAerospikeClient :: Aerospike -> IO (Either AerospikeError ())
connectAerospikeClient (Aerospike as) = alloca @AerospikeError $ \errTmp -> do
    status <- toEnum @AerospikeStatus . fromIntegral <$> [C.exp| int { aerospike_connect($fptr-ptr:(aerospike* as), $(as_error* errTmp)) }|]
    case status of
        AerospikeOk -> return . Right $ ()
        _ -> Left <$> peek errTmp

-- | Should memory from previous fun pointer be free with C hs_free_fun_ptr or Haskell freeHaskellFunPtr?
setLogCallbackAerospike :: (AerospikeLogLevel -> ByteString -> ByteString -> Int -> ByteString -> IO Bool) -> IO ()
setLogCallbackAerospike hsLogFun =
    [C.block| void {
    as_log_hs_callback = (bool (*)(as_log_level, const char*, const char*, uint32_t, const char*))($fun-alloc:(bool (*cLogFun)(int, const char*, const char*, uint32_t, const char*)));
    as_log_set_callback(as_log_hs_callback_wrapper);
}|]
  where
    cLogFun = hsLogFunToC hsLogFun

aerospikeDestroy :: IO (FinalizerPtr Aerospike)
aerospikeDestroy = [C.exp|void (*aerospike_destroy)(aerospike*) { &aerospike_destroy }|]

setLogLevelAerospike :: AerospikeLogLevel -> IO ()
setLogLevelAerospike (fromIntegral . fromEnum -> logLevel) = [C.block|void { as_log_set_level($(int logLevel)); }|]

hsLogFunToC :: (AerospikeLogLevel -> ByteString -> ByteString -> Int -> ByteString -> IO Bool) -> (CInt -> CString -> CString -> Word32 -> CString -> IO CBool)
hsLogFunToC hsLogFun cLogLevel cFunName cFileName cLine cMsg = do
    funcName <- BS.packCString cFunName
    fileName <- BS.packCString cFileName
    msg <- BS.packCString cMsg
    FMU.fromBool
        <$> hsLogFun
            (toEnum . fromIntegral $ cLogLevel)
            funcName
            fileName
            (fromIntegral cLine)
            msg
