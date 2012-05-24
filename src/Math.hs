module Math ( module Prelude
            , div
            , divMod
            ) where

import Prelude ( Int
               , Integer
               , Float
               , Double
               , Rational
               , Num
               , Real
               , Integral
               , Fractional
               , Floating
               , RealFrac
               , RealFloat
               , (+)
               , (-)
               , (*)
               , negate
               , abs
               , signum
               , fromInteger
               , toRational
               , quot
               , rem
               , mod
               , quotRem
               , toInteger
               , (/)
               , recip
               , fromRational
               , pi
               , exp
               , sqrt
               , log
               , (**)
               , logBase
               , sin
               , tan
               , cos
               , asin
               , atan
               , acos
               , sinh
               , tanh
               , cosh
               , asinh
               , atanh
               , acosh
               , properFraction
               , truncate
               , round
               , ceiling
               , floor
               , floatRadix
               , floatDigits
               , floatRange
               , decodeFloat
               , encodeFloat
               , exponent
               , significand
               , scaleFloat
               , isNaN
               , isInfinite
               , isDenormalized
               , isNegativeZero
               , isIEEE
               , atan2
               , subtract
               , even, odd
               , gcd
               , lcm
               , (^)
               , (^^)
               , fromIntegral
               , realToFrac
               )

import Data.Fixed

div :: (Real a, Integral b) => a -> a -> b
div = div'
divMod :: (Real a, Integral b) => a -> a -> (b, a)
divMod = divMod'
