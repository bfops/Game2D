module Main (main) where

import Data.Eq
import Data.Function
import Data.Functor
import Data.Maybe
import System.IO

import Util.Bool
import Util.Monad

import Wrappers.SDL as SDL

checkQuit :: IO Bool
checkQuit = (== Just Quit) <$> pollEvent

main :: IO ()
main = do
        init [ InitVideo ]
        s <- setVideoMode 800 600 32 [ HWSurface, ASyncBlit, NoFrame, DoubleBuf ]
        drawToScreen s
        doWhile (not <$> checkQuit) $ updateRect s $ Rect 0 0 0 0
    where
        drawToScreen s = mapRGB (surfaceGetPixelFormat s) 255 0 0
                       >>= fillRect s (Just $ Rect 0 0 16 16)
