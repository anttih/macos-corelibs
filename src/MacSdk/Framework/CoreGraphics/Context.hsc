module MacSdk.Framework.CoreGraphics.Context where

import Foreign.Ptr (Ptr)
import Foreign.Marshal (with)
import Foreign.C.Types (CDouble(..))
import MacSdk.Framework.CoreFoundation.Object (Object, CFClass)
import MacSdk.Framework.CoreGraphics.Rect (Rect(..))

data CGContext_
type CGContextRef = Ptr CGContext_

foreign import ccall "c_get_current_context" currentCGContext :: IO CGContextRef

type CGFloat = CDouble

data CGColor_
instance CFClass CGColor_
type CGColorRef = Ptr CGColor_
type CGColor = Object CGColor_

foreign import ccall "CGContextSetFillColorWithColor" cgContextSetFillColorWithColor :: CGContextRef -> CGColorRef -> IO ()

setFillColorWithColor :: CGContextRef -> RGBA -> IO ()
setFillColorWithColor contextRef (RGBA { _rgbaRed = red, _rgbaGreen = green, _rgbaBlue = blue, _rgbaAlpha = alpha })= do
  colorRef <- cgColorCreateGenericRGB red green blue alpha
  cgContextSetFillColorWithColor contextRef colorRef

data RGBA = RGBA { _rgbaRed :: CGFloat, _rgbaGreen :: CGFloat, _rgbaBlue :: CGFloat, _rgbaAlpha :: CGFloat }

foreign import ccall "CGColorCreateGenericRGB" cgColorCreateGenericRGB :: CGFloat -> CGFloat -> CGFloat -> CGFloat -> IO CGColorRef

foreign import ccall "CGContextFillRect_" cgContextFillRect :: CGContextRef -> Ptr Rect -> IO ()

fillRect :: CGContextRef -> Rect -> IO ()
fillRect contextRef rect = do
  with rect $ \ptr ->
    cgContextFillRect contextRef ptr