{-# LANGUAGE DeriveGeneric #-}
module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           GHC.Generics
import           Test.QuickCheck
-------------------------------------------------------------------------------
import           Test.QuickCheck.Arbitrary.Generic
-------------------------------------------------------------------------------


main :: IO ()
main = putStrLn "Test suite not yet implemented"


-------------------------------------------------------------------------------
data Foo = Foo | Bar | Baz deriving (Show, Eq, Generic)


instance Arbitrary Foo where
  arbitrary = genericArbitrary


-------------------------------------------------------------------------------
data Foo' = Foo' Int | Bar' Double String deriving (Show, Eq, Generic)

instance Arbitrary Foo' where
  arbitrary = genericArbitrary
