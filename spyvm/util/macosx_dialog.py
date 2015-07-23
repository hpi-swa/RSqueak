from rpython.translator.tool.cbuild import ExternalCompilationInfo
from rpython.translator.platform import CompilationError
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rtyper.tool import rffi_platform as platform
from rpython.rlib.objectmodel import we_are_translated


import py
import sys
import os
from spyvm.util import system

assert system.IS_DARWIN

this_dir = py.path.local(__file__).dirpath()

###############################################################################

eci = ExternalCompilationInfo(
    includes = [str(this_dir.join('macosx/dialog.h'))],
    link_files = [str(this_dir.join('macosx/OpenDialog.m'))],
    frameworks = ['Cocoa', 'AppKit'],
    separate_module_sources = ["""
int irrelevant_caller_of_target_function(char* buffer, int len)
{
  return RSqueakOpenFileDialog_osx(buffer, len);
}
"""]
)

__llget_file = rffi.llexternal('RSqueakOpenFileDialog_osx', [rffi.CCHARP, rffi.INT], rffi.INT, compilation_info=eci)
def _get_file_eci():
    charp = rffi.str2charp("".join(["\0"] * 260))
    res = __llget_file(charp, 260)
    if (res == 1):
        path = rffi.charp2str(charp)
        rffi.free_charp(charp)
        return path
    else:
        return "Squeak.image"

###############################################################################

def _get_file_applepython():
    "NOT_RPYTHON"
    from AppKit import NSOpenPanel, NSOKButton
    panel = NSOpenPanel.openPanel()
    panel.setTitle_("Select a Squeak image file to open")
    panel.setFloatingPanel_(True)
    panel.setOneShot_(True)
    panel.setReleasedWhenClosed_(True)
    panel.setAllowedFileTypes_(["image"])
    panel.center()
    if (panel.runModal() == NSOKButton):
        urls = panel.URLs()
        if (urls.count() ==  1):
            path = urls.objectAtIndex_(0).path()
            try:
                return path.encode('utf_8', 'ignore')
            except ValueError:
                pass
    return "SqueakImage.imgae"

###############################################################################


def get_file():
    if not we_are_translated():
        try:
            from AppKit import NSApplication
            return _get_file_applepython()
        except ImportError:
            pass
    return _get_file_eci()
