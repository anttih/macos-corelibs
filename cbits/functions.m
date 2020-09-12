#include <Carbon/Carbon.h>
#include <AppKit/AppKit.h>

CGError CGWarpMouseCursorPosition_(CGPoint *pos) {
  return CGWarpMouseCursorPosition(*pos);
}

CGEventRef CGEventCreateMouseEvent_(
  CGEventSourceRef source,
  CGEventType mouseType,
  CGPoint *position,
  CGMouseButton mouseButton) {
  return CGEventCreateMouseEvent(source, mouseType, *position, mouseButton);
}

typedef int CGSConnectionID;

extern CGSConnectionID _CGSDefaultConnection(void);
#define CGSDefaultConnection _CGSDefaultConnection()

extern CFStringRef CGSCopyManagedDisplayForWindow(
  const CGSConnectionID Connection, uint32_t WindowId);

void cgDisplayBounds_(uint32_t d_id, CGRect *rect)
{ *rect = CGDisplayBounds(d_id); }

CFStringRef display_for_window(uint32_t window) {
  return CGSCopyManagedDisplayForWindow(CGSDefaultConnection, window);
}

bool get_isback(ProcessSerialNumber *psn) {
  ProcessInfoRec ProcessInfo = {};
  ProcessInfo.processInfoLength = sizeof(ProcessInfoRec);
  GetProcessInformation(psn, &ProcessInfo);
  return (ProcessInfo.processMode & modeOnlyBackground) != 0;
}

typedef OSStatus (carbon_event_callback_t)
  (EventHandlerCallRef HandlerCallRef, EventRef Event, void *Refcon);

EventHandlerUPP handler_upp(carbon_event_callback_t *callb) {
  return NewEventHandlerUPP(callb); }

CGEventMask event_type_to_mask_bit(CGEventType ety) {
  return CGEventMaskBit(ety);
}

void CTRunGetGlyphs_(CTRunRef run, CFRange *range, CGGlyph *buffer) {
  return CTRunGetGlyphs(run, *range, buffer);
}

void CTRunGetPositions_(CTRunRef run, CFRange *range, CGPoint *buffer) {
  return CTRunGetPositions(run, *range, buffer);
}

void CFStringGetCharacters_(CFStringRef s, CFRange *range, UniChar *buffer) {
  return CFStringGetCharacters(s, *range, buffer);
}

CGContextRef c_get_current_context() {
  return NSGraphicsContext.currentContext.CGContext;
}

void CGContextFillRect_(CGContextRef contextRef, CGRect *rect) {
  return CGContextFillRect(contextRef, *rect);
}