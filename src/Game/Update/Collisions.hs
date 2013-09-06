{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Update game state's physical interactions
module Game.Update.Collisions ( update
                              ) where

import Summit.Impure
import Summit.Prelewd
import Summit.Data.Map
import Summit.Data.Member
import Summit.Data.Pair
import Summit.Data.Set
import Summit.Subset.Num

import Text.Show

import Game.Physics
import Game.Object
import Game.Vector
import Physics.Friction
import Physics.Types

keepDims :: Set Dimension -> Vector a -> Vector (Maybe a)
keepDims dims = liftA2 (mcond . (`elem` dims)) dimensions

-- | Transfer some values from one vector to another
substitute :: Set Dimension         -- ^ Which values to substitute from the first vector
           -> Vector a
           -> Vector a
           -> Vector a
substitute dims from to = keepDims dims from <&> (<?>) <*> to

-- | Run both objects' collision functions
collideBoth :: Set Dimension -> Pair Physics -> Pair Velocity
collideBoth dims = collide <*> equilibrium
    where
        collide ps eqTransfer = let toTransfer = eqTransfer
                                             <&> abs
                                             <&> toNat
                                             <&> (<?> error "abs returned negative value")
                                    collided = vcty' . substitute dims . vcty
                                           <$> transfer eqTransfer ps <*> ps
                        in vcty <$> friction (keepDims dims toTransfer) collided

-- | Advance a game state based on collisions
update :: Map (Pair ID) Collisions -> Map ID Physics -> Map ID Velocity
update cs ps = concat $ mapWithKey resultantVPair cs
    where
        resultantVPair ids dims = pairToMap
                                $ zipPair ids
                                $ collideBoth dims (getEntry <$> ids)
        pairToMap = toList >>> fromList
        zipPair = liftA2 (,)
        getEntry i = lookup i ps <?> error ("Could not find ID: " <> show i)
