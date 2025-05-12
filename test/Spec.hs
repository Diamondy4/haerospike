{-# OPTIONS_GHC -Wno-orphans #-}

import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Vector qualified as V
import Database.Aerospike.Record (FromAsBins (..), ToAsBins (..))
import Database.Aerospike.Value (Value (..))
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Hedgehog (Gen, Property, PropertyT, forAll, (===))
import Hedgehog.Gen qualified as Gen
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
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, ToAsBins, FromAsBins)

genFoo :: Gen Foo
genFoo =
    Foo
        <$> Gen.bool
        <*> Gen.int64 constantBounded
        <*> textGen
        <*> Gen.bytes (linear 0 300)
        <*> vecGen (vecGen textGen)
        <*> mapGen ((,) <$> Gen.bytes (linear 0 300) <*> vecGen (Gen.int64 constantBounded))

main :: IO ()
main = do
    hspec $ modifyMaxSuccess (const 1000) $ do
        describe "Aerospike value" $ do
            it "convert between Haskell and C value" $ hedgehog $ do
                x <- forAll genValue
                Just x === valueRoundtrip x

        describe "Convert between Haskell record and list of bins" $ do
            it "Roundtrip for Foo" $ hedgehog $ derivedRoundtrip @Foo genFoo
