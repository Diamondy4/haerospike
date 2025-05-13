{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Exception (SomeException, catch)
import Control.Monad (forM, forM_, void)
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (..))
import Control.Monad.Trans.Resource (MonadResource, register, release, runResourceT)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Int (Int64)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vec.Lazy qualified as A
import Data.Vector qualified as V
import Database.Aerospike (connectAerospikeClient, createAerospikeClient, setLogCallbackAerospike, setLogLevelAerospike)
import Database.Aerospike.Internal.Raw (AerospikeLogLevel (..))
import Database.Aerospike.Key (Key (..), PKey (..))
import Database.Aerospike.Operations (keyBatchedGet, keyGet, keyOperate, keyPut)
import Database.Aerospike.Operator (Operator (..))
import Database.Aerospike.Record (FromAsBins (..), Record (..), ToAsBins (..))
import Database.Aerospike.Value (Value (..))
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Hedgehog (Gen, MonadTest, PropertyT, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (Log (Footnote), failWith, writeLog)
import Hedgehog.Internal.Range (constantBounded, linear)
import Helpers
import Test.Hspec (describe, hspec, it)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

validKey :: Value -> Bool
validKey VNil = False
validKey (VBoolean _) = False
validKey (VMap _) = False
validKey (VList _) = False
validKey _ = True

textGen :: Gen Text
textGen = Gen.text (linear 0 300) (Gen.filter ('\0' /=) Gen.ascii)

vecGen :: Gen a -> Gen (V.Vector a)
vecGen g = V.fromList <$> Gen.list (linear 0 20) g

mapGen :: (Ord k) => Gen (k, v) -> Gen (M.Map k v)
mapGen g = M.fromList <$> Gen.list (linear 0 20) g

genValue :: Gen Value
genValue =
    Gen.recursive
        Gen.choice
        [ Gen.constant VNil
        , VBoolean <$> Gen.bool
        , VInteger <$> Gen.int64 constantBounded
        , VString <$> textGen
        , VBytes <$> Gen.bytes (linear 0 300)
        ]
        [ VList <$> vecGen genValue
        , VMap <$> mapGen ((,) <$> Gen.filter validKey genValue <*> genValue)
        ]

derivedRoundtrip ::
    forall a m.
    ( Monad m
    , ToAsBins a
    , FromAsBins a
    , Show a
    , Eq a
    ) =>
    Gen a ->
    PropertyT m ()
derivedRoundtrip gen = do
    x <- forAll gen
    let bins = toAsBins x
    let fromBins = fromAsBins @a bins
    Just x === fromBins

data Foo = Foo
    { binBool :: Bool
    , binInt :: Int64
    , binText :: Text
    , binBytes :: BS.ByteString
    , binList :: V.Vector (V.Vector Text)
    , binMap :: M.Map BS.ByteString (V.Vector Int64)
    }
    deriving stock (Show, Eq, GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToAsBins, FromAsBins) via (GHC.Generically Foo)

genFoo :: Gen Foo
genFoo =
    Foo
        <$> Gen.bool
        <*> Gen.int64 constantBounded
        <*> textGen
        <*> Gen.bytes (linear 0 300)
        <*> vecGen (vecGen textGen)
        <*> mapGen ((,) <$> Gen.bytes (linear 0 300) <*> vecGen (Gen.int64 constantBounded))

type RawBins = [(BS.ByteString, Value)]

genBinName :: Gen BS.ByteString
genBinName = encodeUtf8 <$> Gen.text (linear 1 15) Gen.alpha

genBins :: Gen RawBins
genBins = M.assocs . M.fromList <$> Gen.list (linear 1 20) ((,) <$> genBinName <*> Gen.filter (VNil /=) genValue)

main :: IO ()
main = do
    let ip = "127.0.0.1" :: BS.ByteString
    as <- createAerospikeClient ip 3000 100
    setLogCallbackAerospike (\a b c d e -> print (a, b, c, d, e) >> pure True)
    setLogLevelAerospike AsLogLevelTrace
    conRes <- connectAerospikeClient as
    print conRes

    hspec $ do
        modifyMaxSuccess (const 1000) $ describe "Aerospike value" $ do
            it "convert between Haskell and C value" $ hedgehog $ do
                x <- forAll genValue
                Just x === valueRoundtrip x

        modifyMaxSuccess (const 100) $ describe "Convert between Haskell record and list of bins" $ do
            it "Roundtrip for Foo" $ hedgehog $ derivedRoundtrip @Foo genFoo

        describe "Aerospike operations" $ do
            let ns = "test" :: BS.ByteString
            let set = "test_set" :: BS.ByteString

            let
                withKeys :: forall n m. (MonadResource m, MonadIO m) => A.Vec n Text -> (A.Vec n Key -> m ()) -> m ()
                withKeys keys f = do
                    let keys' = fmap (MkKey ns set . KString) keys
                    releaseKeys <- forM keys' $ \key -> register . void $ keyOperate @RawBins as key [Delete]
                    f keys'
                    forM_ releaseKeys release

            let
                keyPut' :: forall m. (MonadIO m, MonadTest m) => Key -> RawBins -> m ()
                keyPut' k b = do
                    resPut <- liftIO (keyPut as k b)
                    assert $ isRight resPut

                keyGet' :: forall a m. (FromAsBins a, MonadIO m, MonadTest m) => Key -> m (Record a)
                keyGet' key = do
                    res <- liftIO $ keyGet @a as key
                    case res of
                        Right (Just r) -> pure r
                        _ -> failWith Nothing "Failed to get value from aerospike"

            it "Simple put get roundtrip" $ hedgehog . hoist runResourceT $ do
                bins <- forAll genBins
                withKeys ("simple" A.::: A.VNil) $ \(key A.::: A.VNil) -> do
                    keyPut' key bins
                    r <- keyGet' @RawBins key
                    bins === r.bins

            it "Simple put batch roundtrip" $ hedgehog . hoist runResourceT $ do
                bins1 <- forAll genBins
                bins2 <- forAll genBins
                bins3 <- forAll genBins

                withKeys ("simple1" A.::: "simple2" A.::: "simple3" A.::: A.VNil) $ \(key1 A.::: key2 A.::: key3 A.::: A.VNil) -> do
                    keyPut' key1 bins1
                    keyPut' key2 bins2
                    keyPut' key3 bins3

                    res <- liftIO $ keyBatchedGet @RawBins as [key1, key2, key3]
                    let res' = eitherToMaybe $ fmap (fmap (.bins) . catMaybes) res

                    Just [bins1, bins2, bins3] === res'
