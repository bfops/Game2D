module Game.Physics ( Physics (..)
                    , size'
                    , mass'
                    , posn'
                    , vcty'
                    , accl'
                    , mu'
                    ) where

import Prelewd

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
    arbitrary = Physics <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
