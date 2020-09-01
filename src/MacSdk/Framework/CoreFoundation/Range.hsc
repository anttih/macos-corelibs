module MacSdk.Framework.CoreFoundation.Range where

import Foreign.Storable (Storable, peek, poke, alignment, sizeOf, peekByteOff, pokeByteOff)
import MacSdk.Framework.CoreFoundation.Array (CFIndex)

#include <Carbon/Carbon.h>

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