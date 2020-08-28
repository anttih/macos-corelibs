module MacSdk.Framework.CoreFoundation.AttributedString where

import Control.Monad.Managed
import Foreign.Storable (Storable, peek, poke, alignment, sizeOf, peekByteOff, pokeByteOff)
import Foreign.Ptr (Ptr)
import MacSdk.Framework.CoreFoundation.Allocator (Allocator, CFAllocatorRef)
import MacSdk.Framework.CoreFoundation.String (CFStringRef, CFStringEncoding(..))
import qualified MacSdk.Framework.CoreFoundation.String as String
import MacSdk.Framework.CoreFoundation.Array (CFIndex)
import MacSdk.Framework.CoreFoundation.Dictionary (CFDictionaryRef, Dictionary)
import MacSdk.Framework.CoreFoundation.Object (withCFPtr, manageCFObj, Object, CFClass)

#include <Carbon/Carbon.h>

data CFAttributedString_
instance CFClass CFAttributedString_
type CFAttributedStringRef = Ptr CFAttributedString_

type CFAttributedString = Object CFAttributedString_

foreign import ccall "CFAttributedStringCreate" cfAttributedStringCreate
  :: CFAllocatorRef -> CFStringRef -> CFDictionaryRef -> IO CFAttributedStringRef

fromString :: Allocator -> String -> Dictionary -> IO CFAttributedString
fromString alloc s dict = liftIO $ flip with pure $ do
  d' <- managed (withCFPtr dict)
  alloc' <- managed (withCFPtr alloc)
  cfString <- liftIO (String.fromString CFStringEncodingASCII s)
  cfString' <- managed (withCFPtr cfString)
  liftIO $ do
    cfAttributedStringCreate alloc' cfString' d' >>= manageCFObj

data Range = Range { cfRangeLength :: CFIndex, cfRangeLocation :: CFIndex }

instance Storable Range where
  sizeOf _ = #{size CFRange}
  alignment _ = #{alignment CFRange}
  peek ptr = do
    length' <- #{peek CFRange, length} ptr
    location' <- #{peek CFRange, location} ptr
    pure (Range length' location')
  poke ptr (Range length' location') = do
    #{poke CFRange, length} ptr length'
    #{poke CFRange, location} ptr location'
