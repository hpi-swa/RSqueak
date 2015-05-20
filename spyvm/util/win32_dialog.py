from rpython.translator.tool.cbuild import ExternalCompilationInfo
from rpython.translator.platform import CompilationError
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rtyper.tool import rffi_platform as platform
import py
import sys
import os
from spyvm.util import system

assert system.IS_WINDOWS

this_dir = os.path.dirname(__file__)
eci = ExternalCompilationInfo(
        post_include_bits = ["""
#ifndef opendialog_h
#define opendialog_h

#include <windows.h>
#include <Commdlg.h>

#ifdef __cplusplus
extern "C" {
#endif
	__declspec(dllexport) int RSqueakOpenFileDialog(char*, int);
#ifdef __cplusplus
}
#endif

#endif"""],
        include_dirs = [this_dir],
        link_files = ['comdlg32.lib'],
        separate_module_sources = ["""
int RSqueakOpenFileDialog(char* szFile, int len) {
	 OPENFILENAME ofn;
     int res;
	 ZeroMemory(&ofn, sizeof(ofn));
	 ofn.lStructSize = sizeof(ofn);
	 ofn.hwndOwner = NULL;
	 ofn.lpstrFile = (LPSTR)szFile;
	 ofn.lpstrFile[0] = '\\0';
	 ofn.nMaxFile = len - 1;
	 ofn.lpstrFilter = (LPSTR)"All\\0*.*\\0Images\\0*.image\\0";
	 ofn.nFilterIndex = 2;
	 ofn.lpstrFileTitle = NULL;
	 ofn.nMaxFileTitle = 0;
	 ofn.lpstrInitialDir = NULL;
	 ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;
	 res = (int)GetOpenFileNameA(&ofn);
	 return res;
}"""]
)

__llget_file = rffi.llexternal('RSqueakOpenFileDialog', [rffi.CCHARP, rffi.INT], rffi.INT, compilation_info=eci)
def get_file():
    charp = rffi.str2charp("".join(["\0"] * 260))
    res = __llget_file(charp, 260)
    if (res == 1):
        path = rffi.charp2str(charp)
        rffi.free_charp(charp)
        return path
    else:
        return "Squeak.image"
