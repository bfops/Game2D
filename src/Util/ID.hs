{-# LANGUAGE NoImplicitPrelude
           #-}
module Util.ID ( ID
               , Named
               ) where

import Prelewd

import Storage.Map

type ID = Integer
type Named = Map Integer
