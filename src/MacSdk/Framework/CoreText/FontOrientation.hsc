module MacSdk.Framework.CoreText.FontOrientation where

import Data.Word (Word32)

#include <Carbon/Carbon.h>

data CTFontOrientation
  = CTFontOrientationDefault
  | CTFontOrientationHorizontal
  | CTFontOrientationVertical

type ForeignCTFontOrientation = Word32

toForeignEncoding :: CTFontOrientation -> ForeignCTFontOrientation
toForeignEncoding = \case
  CTFontOrientationDefault -> (#const kCTFontOrientationDefault)
  CTFontOrientationHorizontal -> (#const kCTFontOrientationHorizontal)
  CTFontOrientationVertical -> (#const kCTFontOrientationVertical)
