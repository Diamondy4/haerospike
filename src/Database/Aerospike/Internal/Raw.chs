{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Aerospike.Internal.Raw where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

#include <aerospike/aerospike.h>
#include <aerospike/as_status.h>

{# context lib="aerospike" #}

{# enum as_status as AerospikeStatus {underscoreToCase} deriving (Eq, Show) #}

{# pointer *aerospike as Aerospike foreign newtype #}

data AerospikeError = AerospikeError
  { code :: AerospikeStatus
  , file :: ByteString
  , func :: ByteString
  , inDoubt :: Bool
  , line :: Int
  , message :: ByteString
  } deriving (Show)

bsToCString :: ByteString -> IO CString
bsToCString bs = BS.useAsCString bs {# call unsafe strdup #}
{-# INLINE bsToCString #-}

instance Storable AerospikeError where
  sizeOf _ = {#sizeof as_error#}
  alignment _ = {#alignof as_error#}
  poke p v = do
    {#set as_error.code#} p (fromIntegral . fromEnum $ code v)
    {#set as_error.file#} p =<< bsToCString (file v)
    {#set as_error.func#} p =<< bsToCString (func v)
    {#set as_error.in_doubt#} p (inDoubt v)
    {#set as_error.line#} p (fromIntegral $ line v)
    {#set as_error.message#} p =<< (bsToCString . BS.take ({#const AS_ERROR_MESSAGE_MAX_SIZE#} - 2) $ message v)
  peek p = do
      code <- toEnum @AerospikeStatus . fromIntegral <$> {#get as_error.code#} p
      file <- BS.packCString =<< {#get as_error.file#} p
      func <- BS.packCString =<< {#get as_error.func#} p
      inDoubt <- {#get as_error.in_doubt#} p
      line <- fromIntegral <$> {#get as_error.line#} p
      message <- {#get as_error.message#} p >>= \messagePtr -> BS.packCString messagePtr
      pure $ AerospikeError {..}

