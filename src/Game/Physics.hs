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
import Test.QuickCheck (Arbitrary (..))
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
    -- Anything with Infinite mass isn't allowed to accelerate
    arbitrary = do m <- arbitrary
                   Physics <$> arbitrary <*> pure m <*> arbitrary <*> arbitrary <*> inner m <*> arbitrary
        where
            inner Infinite = pure 0
            inner _ = arbitrary
