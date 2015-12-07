{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
module Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    ) where


-------------------------------------------------------------------------------
-- import           Control.Applicative
import           GHC.Generics
import           Test.QuickCheck
-------------------------------------------------------------------------------



genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = to <$> gArbitrary


-------------------------------------------------------------------------------
class GArbitrary f where
  gArbitrary :: Gen (f a)


-- constructor with no fields
instance GArbitrary U1 where
  gArbitrary = pure U1


-- product type, arbitrary over each field
instance (GArbitrary a, GArbitrary b) => GArbitrary (a :*: b) where
  gArbitrary = (:*:) <$> gArbitrary <*> gArbitrary


-- sum type, arbitrary over reach constructor
instance (GArbitrary a, GArbitrary b) => GArbitrary (a :+: b) where
  gArbitrary = oneof [L1 <$> gArbitrary, R1 <$> gArbitrary]


instance (Arbitrary a) => GArbitrary (K1 i a) where
  gArbitrary = K1 <$> arbitrary


instance (GArbitrary f) => GArbitrary (M1 i c f) where
  gArbitrary = M1 <$> gArbitrary
