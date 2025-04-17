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
import Language.C.Types qualified as C
import Language.Haskell.TH qualified as TH

asCtx :: Context
asCtx = mempty{ctxTypesTable = asTypesTable}

asTypesTable :: Map C.TypeSpecifier TH.TypeQ
asTypesTable =
    [ (C.TypeName "aerospike", [t|Aerospike|])
    , (C.TypeName "as_batch_records", [t|AsBatchRecords|])
    , (C.TypeName "as_operations", [t|AsOperations|])
    , (C.TypeName "as_record", [t|AsRecord|])
    , (C.TypeName "as_bin_value", [t|AsBinValue|])
    , (C.TypeName "as_val", [t|AsVal|])
    , (C.TypeName "as_list", [t|AsList|])
    , (C.TypeName "as_map", [t|AsMap|])
    , (C.TypeName "as_key", [t|AsKey|])
    , (C.TypeName "as_error", [t|AerospikeError|])
    , (C.TypeName "as_log_level", [t|AerospikeLogLevel|])
    ]
