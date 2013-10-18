{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.State( GameState
                 ) where

import Game.Object

import Util.ID

type GameState = Named (GameObject, ObjectBehavior)
