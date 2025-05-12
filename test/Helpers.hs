module Helpers where

import Control.Monad.Cont
import Control.Monad.Trans
import Database.Aerospike.Internal.Raw
import Database.Aerospike.TypesBridge
import Database.Aerospike.Value (Value)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import GHC.IO (unsafePerformIO)

{-# NOINLINE valueRoundtrip #-}
valueRoundtrip :: Value -> Maybe Value
valueRoundtrip val = unsafePerformIO $ evalContT $ do
    cVal <- createVal val
    cVal' <- lift $ AsVal <$> newForeignPtr_ cVal
    lift $ parseBinValue cVal'
