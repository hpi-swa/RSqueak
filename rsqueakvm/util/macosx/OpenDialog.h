#import <Cocoa/Cocoa.h>
#import <AppKit/AppKit.h>

@interface OpenDialog : NSObject
- (int) openFileDialogResult: (char*) res length: (int) len;
@end
