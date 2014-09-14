module HandleFixedWindows (isNonResizable) where

import XMonad
import XMonad.Util.WindowProperties
import XMonad.Hooks.ManageHelpers
import Foreign.C.Types (CLong)

type XSizeHints = [CLong]

minWidth, minHeight, maxWidth, maxHeight :: XSizeHints -> CLong
minWidth  = (!! 5)
minHeight = (!! 6)
maxWidth  = (!! 7)
maxHeight = (!! 8)

hasFixedSize :: XSizeHints -> Bool
hasFixedSize h = minWidth h == maxWidth h
              && minHeight h == maxHeight h
              && all (>0) [minWidth h, maxWidth h, minHeight h, maxHeight h]

isNonResizable :: Query Bool
isNonResizable = ask >>= \w -> liftX $ do
    atom <- getProp32s "WM_NORMAL_HINTS" w
    return $ case atom of
        Just hints  -> hasFixedSize hints
        Nothing     -> False
