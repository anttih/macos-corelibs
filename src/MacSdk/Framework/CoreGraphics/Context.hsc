module MacSdk.Framework.CoreGraphics.Context where

import Foreign.Ptr (Ptr)

data CGContext_
type CGContextRef = Ptr CGContext_

foreign import ccall "c_get_current_context" currentCGContext :: IO CGContextRef