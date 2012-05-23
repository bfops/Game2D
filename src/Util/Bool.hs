module Util.Bool ( module B
                 , iff
                 , if'
                 , bool
                 ) where

import Data.Bool as B

-- | Process conditionals the same way as `maybe` and `either`.
bool :: a    -- ^ Return if false
     -> a    -- ^ Return if true
     -> Bool
     -> a
bool f t b = iff b t f

-- | Function for `if.. then.. else ..`
iff :: Bool -> a -> a -> a
iff True t _ = t
iff False _ f = f

-- | Conditionally apply a transformation function
if' :: (a -> a) -> a -> Bool -> a
if' f x True = f x
if' _ x False = x
