import os, sys, subprocess
import py
from rpython.translator.tool.cbuild import ExternalCompilationInfo
from rpython.rtyper.lltypesystem import rffi
from rsqueakvm.util import system

_Default = "Squeak.image"
this_dir = py.path.local(__file__).dirpath()

if system.IS_WINDOWS:
    lfiles = ['comdlg32.lib', 'ole32.lib', 'shell32.lib', 'user32.lib']
else:
    lfiles = [str(this_dir.join("tinyfiledialogs/tinyfiledialogs.c"))]

eci = ExternalCompilationInfo(
        include_dirs=[this_dir],
        link_files=lfiles,
        separate_module_sources=["""
            #include <string.h>
            #ifdef _WIN32
            #include <windows.h>
            #include <Commdlg.h>
            #ifndef _tinyfd
            #define _tinyfd
            #include "tinyfiledialogs/tinyfiledialogs.c"
            #endif
            #define DLLEXPORT __declspec(dllexport)
            #else
            #ifndef _tinyfd
            #define _tinyfd
            #include "tinyfiledialogs/tinyfiledialogs.h"
            #endif
            #include <sys/time.h>
            #include <sys/resource.h>
            #define DLLEXPORT __attribute__((__visibility__("default")))
            #endif

            DLLEXPORT int RSqueakOpenFileDialog(char* szFile, int len) {
                char const * const filter = "*.image";
                const char * file = tinyfd_openFileDialog("", "", 1, &filter, 0, 0);
                if (file != 0) {
                    strncpy(szFile, file, len - 1);
                }
                return (file == 0) ? 0: 1;
            }

            DLLEXPORT int RSqueakAskQuestion(const char* question) {
                return tinyfd_messageBox("RSqueak", question, "yesno", "question", 1);
            }
            """]
)


if not system.IS_ARM and (system.IS_WINDOWS or system.IS_LINUX or system.IS_DARWIN):
    __llget_file = rffi.llexternal('RSqueakOpenFileDialog',
                                   [rffi.CCHARP, rffi.INT], rffi.INT,
                                   compilation_info=eci)
    __llask_question = rffi.llexternal('RSqueakAskQuestion',
                                       [rffi.CCHARP], rffi.INT,
                                       compilation_info=eci)
else:
    def __llget_file(x, y): return _Default
    def __llask_question(x): return 1


def get_file():
    with rffi.scoped_str2charp("".join(["\0"] * 260)) as charp:
        res = __llget_file(charp, 260)
        if res == 1:
            return rffi.charp2str(charp)
    return _Default


def ask_question(string):
    with rffi.scoped_view_charp(string) as charp:
        res = __llask_question(charp)
        return res == 1
    return True
