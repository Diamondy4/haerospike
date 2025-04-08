{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Database.Aerospike.Internal where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Database.Aerospike.Internal.Raw
import Foreign
import Language.C.Inline.Context
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

asCtx :: Context
asCtx = mempty{ctxTypesTable = asTypesTable}

asTypesTable :: Map C.TypeSpecifier TH.TypeQ
asTypesTable =
    [ (C.TypeName "aerospike", [t|Aerospike|])
    , (C.TypeName "as_batch_records", [t|AsBatchRecords|])
    , (C.TypeName "as_error", [t|AerospikeError|])
    , (C.TypeName "as_log_level", [t|AerospikeLogLevel|])
    ]
