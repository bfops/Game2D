module Main (main) where

import Data.Eq
import Data.Function
import Data.Functor
import System.IO

import Util.Bool
import Util.Monad

import Graphics.UI.SDL as SDL

checkQuit :: IO Bool
checkQuit = (== Quit) <$> pollEvent

main :: IO ()
main = do
    init [ InitVideo ]
    _ <- setVideoMode 800 600 32 [ HWSurface, ASyncBlit, NoFrame, DoubleBuf ]
    loop $ not <$> checkQuit
