module MacSdk.Framework.CoreText where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..), CSize(..), CShort(..), CBool(..))
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (managed, with)
import MacSdk.Framework.CoreGraphics
import MacSdk.Framework.CoreFoundation.AttributedString (CFAttributedString, CFAttributedStringRef, Range)
import MacSdk.Framework.CoreFoundation.Array (CFArrayRef, CFIndex)
import MacSdk.Framework.CoreFoundation.Object (Object, CFClass, withCFPtr, manageCFObj)
import MacSdk.Framework.CoreFoundation.String (CFStringRef)

#include <CoreText/CoreText.h>

data CTLine_
instance CFClass CTLine_
type CTLineRef = Ptr CTLine_

type CTLine = Object CTLine_

foreign import ccall "CTLineCreateWithAttributedString" ctLineCreateWithAttributedString :: CFAttributedStringRef -> IO CTLineRef

lineCreateWithAttributedString :: CFAttributedString -> IO CTLine
lineCreateWithAttributedString attrStr = liftIO $ flip with pure $ do
  attrStrRef <- managed (withCFPtr attrStr)
  liftIO (ctLineCreateWithAttributedString attrStrRef >>= manageCFObj)

foreign import ccall "CTLineGetGlyphRuns" ctLineGetGlyphRuns :: CTLineRef -> IO CFArrayRef

data CTRun_
instance CFClass CTRun_
type CTRunRef = Ptr CTRun_
type CTRun = Object CTRun_

data CTFont_
instance CFClass CTFont_
type CTFontRef = Ptr CTFont_
type CTFont = Object CTFont_

foreign import ccall "CTFontGetGlyphsForCharacters" ctFontGetGlyphsForCharacters
  :: CTFontRef -> Ptr CShort -> Ptr CGGlyph -> CFIndex -> IO CBool

foreign import ccall "CTRunGetGlyphCount" ctRunGetGlyphCount :: CTRunRef -> IO CInt

foreign import ccall "CTRunGetGlyphs_" ctRunGetGlyphs :: CTRunRef -> Ptr Range -> Ptr CGGlyph -> IO ()

foreign import ccall "CTRunGetPositions_" ctRunGetPositions :: CTRunRef -> Ptr Range -> Ptr Point -> IO ()

foreign import ccall "CTFontDrawGlyphs" ctFontDrawGlyphs
  :: CTFontRef -> Ptr CGGlyph -> Ptr Point -> CSize -> CGContextRef -> IO ()

data FontAttribute
  = CTFontAttributeName
  | CTKernAttributeName
  | CTLigatureAttributeName
  | CTForegroundColorAttributeName
  | CTForegroundColorFromContextAttributeName
  | CTParagraphStyleAttributeName
  | CTStrokeWidthAttributeName
  | CTStrokeColorAttributeName
  | CTSuperscriptAttributeName
  | CTUnderlineColorAttributeName
  | CTUnderlineStyleAttributeName
  | CTVerticalFormsAttributeName
  | CTGlyphInfoAttributeName
  | CTRunDelegateAttributeName
  -- | CTBaselineOffsetAttributeName
  -- | CTTrackingAttributeName

type CTFontAttribute = CFStringRef

-- https://developer.apple.com/documentation/coretext/styling_attributed_strings/string_attribute_name_constants?language=objc
foreign import ccall unsafe kCTFontAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTKernAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTLigatureAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTForegroundColorAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTForegroundColorFromContextAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTParagraphStyleAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTStrokeWidthAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTStrokeColorAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTSuperscriptAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTUnderlineColorAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTUnderlineStyleAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTVerticalFormsAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTGlyphInfoAttributeName :: IO CTFontAttribute
foreign import ccall unsafe kCTRunDelegateAttributeName :: IO CTFontAttribute

toForeignCTFontAttribute :: FontAttribute -> CTFontAttribute
toForeignCTFontAttribute = unsafePerformIO . \case
  CTFontAttributeName -> kCTFontAttributeName
  CTKernAttributeName -> kCTKernAttributeName
  CTLigatureAttributeName -> kCTLigatureAttributeName
  CTForegroundColorAttributeName -> kCTForegroundColorAttributeName
  CTForegroundColorFromContextAttributeName -> kCTForegroundColorFromContextAttributeName
  CTParagraphStyleAttributeName -> kCTParagraphStyleAttributeName
  CTStrokeWidthAttributeName -> kCTStrokeWidthAttributeName
  CTStrokeColorAttributeName -> kCTStrokeColorAttributeName
  CTSuperscriptAttributeName -> kCTSuperscriptAttributeName
  CTUnderlineColorAttributeName -> kCTUnderlineColorAttributeName
  CTUnderlineStyleAttributeName -> kCTUnderlineStyleAttributeName
  CTVerticalFormsAttributeName -> kCTVerticalFormsAttributeName
  CTGlyphInfoAttributeName -> kCTGlyphInfoAttributeName
  CTRunDelegateAttributeName -> kCTRunDelegateAttributeName
  -- CTBaselineOffsetAttributeName -> (#const kCTBaselineOffsetAttributeName)
  -- CTTrackingAttributeName -> (#const kCTTrackingAttributeName)