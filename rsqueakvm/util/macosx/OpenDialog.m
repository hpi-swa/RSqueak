/*
 *
 *
 *
 */
#include "dialog.h"
#import "OpenDialog.h"

int RSqueakOpenFileDialog_osx(char* buffer, int len)
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  OpenDialog* odl = [[OpenDialog new] autorelease];
  int res = [odl openFileDialogResult: buffer length: len];

  [pool drain];
  return res;
}


@implementation OpenDialog
- (int) openFileDialogResult: (char*) buffer length: (int) len
{
  int ret = 0;
  NSOpenPanel* panel= [NSOpenPanel openPanel];
  NSArray* types = [[NSArray alloc] initWithObjects: @"image", nil ];
  [panel setTitle: @"Select a Squeak image file to open"];
  [panel setFloatingPanel: YES];
  [panel setOneShot: YES];
  [panel setReleasedWhenClosed: YES];
  [panel setAllowedFileTypes: types];

  [panel center];

  if (NSOKButton == [panel runModal]) {
    NSArray *urls = [panel URLs];
    if (1 == [urls count]) {
      NSString* path = [[urls objectAtIndex: 0] path];
      if ([path length] < len) {
        ret = 1;
        BOOL ok = [path getCString: buffer
                         maxLength: len - 1
                          encoding: NSUTF8StringEncoding];
        if (ok) {
          ret = 1;
        }
      }
    }
  }

  [types release];
  return ret;
}
@end
