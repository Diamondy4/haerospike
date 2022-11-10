{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Database.Aerospike where

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
    asFinalizer <- aerospikeDestroy
    Aerospike <$> newForeignPtr asFinalizer as

connectAerospikeClient :: Aerospike -> IO (Either AerospikeError ())
connectAerospikeClient (Aerospike as) = alloca @AerospikeError $ \errTmp -> do
    status <- toEnum @AerospikeStatus . fromIntegral <$> [C.exp| int { aerospike_connect($fptr-ptr:(aerospike* as), $(as_error* errTmp)) }|]
    case status of
        AerospikeOk -> return . Right $ ()
        _ -> Left <$> peek errTmp

aerospikeDestroy :: IO (FinalizerPtr Aerospike)
aerospikeDestroy = [C.exp|void (*aerospike_destroy)(aerospike*) { &aerospike_destroy }|]