{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Aerospike.Policy.CBridge where

import Control.Monad (forM_)
import Database.Aerospike.Internal
import Database.Aerospike.Internal.Raw
import Database.Aerospike.Policy.Policy
import Foreign
import Foreign.C
import Language.C.Inline qualified as C

C.context (C.baseCtx <> C.bsCtx <> C.fptrCtx <> asCtx <> C.vecCtx <> C.funCtx)
C.include "<aerospike/as_policy.h>"
C.include "<string.h>"

initBasePolicy :: Ptr AsPolicyBase -> BasePolicy -> IO ()
initBasePolicy policyPtr policy = do
    [C.block| void {
        as_policy_base* p = $(as_policy_base* policyPtr);
        memset(p, 0, sizeof(as_policy_base));
        p->socket_timeout = AS_POLICY_SOCKET_TIMEOUT_DEFAULT;
	    p->total_timeout = AS_POLICY_TOTAL_TIMEOUT_DEFAULT;
    }|]

    forM_ policy.socketTimeout $ \v -> do
        [C.block| void {
            $(as_policy_base* policyPtr)->socket_timeout = $(uint32_t v);
        }|]

    forM_ policy.totalTimeout $ \v -> do
        [C.block| void {
            $(as_policy_base* policyPtr)->total_timeout = $(uint32_t v);
        }|]

    forM_ policy.maxRetries $ \v -> do
        [C.block| void {
            $(as_policy_base* policyPtr)->max_retries = $(uint32_t v);
        }|]

    forM_ policy.sleepBetweenRetires $ \v -> do
        [C.block| void {
            $(as_policy_base* policyPtr)->sleep_between_retries = $(uint32_t v);
        }|]

initKeyPolicy :: Ptr AsPolicyKey -> KeyPolicy -> IO ()
initKeyPolicy policyPtr = \case
    KeyDigest -> [C.block| void { *$(as_policy_key* policyPtr) = AS_POLICY_KEY_DIGEST; }|]
    KeySend -> [C.block| void { *$(as_policy_key* policyPtr) = AS_POLICY_KEY_SEND; }|]

initReadPolicy :: Ptr AsPolicyRead -> ReadPolicy -> IO ()
initReadPolicy policyPtr policy = do
    basePtr <-
        [C.block| as_policy_base* {
        as_policy_base* base = &$(as_policy_read* policyPtr)->base;
        as_policy_base_read_init(base);
        return base;
    }|]

    initBasePolicy basePtr policy.base
    keyPolicyPtr <- [C.exp| as_policy_key* { &$(as_policy_read* policyPtr)->key }|]
    initKeyPolicy keyPolicyPtr policy.key

initExistsPolicy :: Ptr AsPolicyExists -> ExistsPolicy -> IO ()
initExistsPolicy policyPtr = \case
    ExistsIgnore -> [C.block| void { *$(as_policy_exists* policyPtr) = AS_POLICY_EXISTS_IGNORE; }|]
    ExistsCreate -> [C.block| void { *$(as_policy_exists* policyPtr) = AS_POLICY_EXISTS_CREATE; }|]
    ExistsUpdate -> [C.block| void { *$(as_policy_exists* policyPtr) = AS_POLICY_EXISTS_UPDATE; }|]
    ExistsReplace -> [C.block| void { *$(as_policy_exists* policyPtr) = AS_POLICY_EXISTS_REPLACE; }|]
    ExistsCreateOrReplace -> [C.block| void { *$(as_policy_exists* policyPtr) = AS_POLICY_EXISTS_CREATE_OR_REPLACE; }|]

initWritePolicy :: Ptr AsPolicyWrite -> WritePolicy -> IO ()
initWritePolicy policyPtr policy = do
    basePtr <-
        [C.block| as_policy_base* {
        as_policy_base* base = &$(as_policy_write* policyPtr)->base;
        as_policy_base_write_init(base);
        return base;
    }|]

    initBasePolicy basePtr policy.base
    keyPolicyPtr <- [C.exp| as_policy_key* { &$(as_policy_write* policyPtr)->key }|]
    initKeyPolicy keyPolicyPtr policy.key
    existsPolicyPtr <- [C.exp| as_policy_exists* { &$(as_policy_write* policyPtr)->exists }|]
    initExistsPolicy existsPolicyPtr policy.exists

initBatchPolicy :: Ptr AsPolicyBatch -> BatchPolicy -> IO ()
initBatchPolicy policyPtr policy = do
    let concurrent = fromBool @CBool policy.concurrent
    let allowInline = fromBool @CBool policy.allowInline
    let allowInlineSsd = fromBool @CBool policy.allowInlineSsd
    let respondAllKeys = fromBool @CBool policy.respondAllKeys
    let deserialize = fromBool @CBool policy.deserialize

    basePtr <-
        [C.block| as_policy_base* {
        as_policy_batch* p = $(as_policy_batch* policyPtr);
        as_policy_batch_init(p);

        p->concurrent = $(bool concurrent);
        p->allow_inline = $(bool allowInline);
        p->allow_inline_ssd = $(bool allowInlineSsd);
        p->respond_all_keys = $(bool respondAllKeys);
        p->deserialize = $(bool deserialize);
        
        return &p->base;
    }|]

    initBasePolicy basePtr policy.base

initOperatePolicy :: Ptr AsPolicyOperate -> OperatePolicy -> IO ()
initOperatePolicy policyPtr policy = do
    let deserialize = fromBool @CBool policy.deserialize
    let respondAllOps = fromBool @CBool policy.respondAllOps

    basePtr <-
        [C.block| as_policy_base* {
        as_policy_operate* p = $(as_policy_operate* policyPtr);
        as_policy_operate_init(p);

        p->deserialize = $(bool deserialize);
        p->respond_all_ops = $(bool respondAllOps);
        return &p->base;
    }|]

    initBasePolicy basePtr policy.base
    keyPolicyPtr <- [C.exp| as_policy_key* { &$(as_policy_operate* policyPtr)->key }|]
    initKeyPolicy keyPolicyPtr policy.key
    existsPolicyPtr <- [C.exp| as_policy_exists* { &$(as_policy_operate* policyPtr)->exists }|]
    initExistsPolicy existsPolicyPtr policy.exists
