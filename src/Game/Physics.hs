{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
module Game.Physics ( Physics (..)
                    , size'
                    , mass'
                    , posn'
                    , vcty'
                    , accl'
                    , mu'
                    ) where

import Prelewd

import Num.Nonfinite
import Template.MemberTransformer
import Test.QuickCheck (Arbitrary (..), suchThat)
import Text.Show

import Game.Vector
import Physics.Types

-- | Collection of physical properties for an object
data Physics = Physics
        { size :: Size
        , mass :: Mass
        , posn :: Position
        , vcty :: Velocity
        , accl :: Vector Acceleration
        , mu   :: Mu
        }
    deriving (Show, Eq)

$(memberTransformers ''Physics)

instance Arbitrary Physics where
    -- Anything with Infinite mass is _NOT_ allowed to accelerate.
    arbitrary = inner `suchThat` (\p -> mass p < Infinite || accl p == 0)
        where inner = Physics <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
