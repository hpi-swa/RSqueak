import os, sys, subprocess
import py
from rpython.translator.tool.cbuild import ExternalCompilationInfo
from rpython.rtyper.lltypesystem import rffi
from rsqueakvm.util import system

assert system.IS_LINUX

this_dir = py.path.local(__file__).dirpath()

###############################################################################

_Default = "Squeak.image"

if subprocess.call("fltk-config --version", shell=True) == 0:
    cflags = subprocess.check_output("fltk-config --cxxflags", shell=True).strip().split()
    ldflags = subprocess.check_output("fltk-config --ldstaticflags", shell=True).strip().split()
    eci = ExternalCompilationInfo(
        includes=[str(this_dir.join('linux/file_chooser.h'))],
        link_files=[str(this_dir.join('linux/file_chooser.cxx'))],
        libraries=["stdc++"],
        compile_extra=cflags,
        link_extra=ldflags,
        separate_module_sources=["""
        int irrelevant_caller_of_target_function(char* buffer, int len)
        {
            return RSqueakOpenFileDialog_linux(buffer, len);
        }
        """]
    )

    __llget_file = rffi.llexternal('RSqueakOpenFileDialog_linux',
                                   [rffi.CCHARP, rffi.INT], rffi.INT,
                                   compilation_info=eci)
    def fltk_get_file():
        charp = rffi.str2charp("".join(["\0"] * 260))
        res = __llget_file(charp, 260)
        if (res == 1):
            path = rffi.charp2str(charp)
            rffi.free_charp(charp)
            return path
        else:
            return _Default
else:
    print "\033[33;5;7m You do not have FLTK installed - file picker will have no fall back if Zenity is not available at runtime \033[0m"
    def fltk_get_file():
        print "\033[33;5;7m You do not have Zenity installed - file picker not available \033[0m"
        return _Default

def get_file():
    r, w = os.pipe()
    pid = os.fork()
    if pid == 0:
        os.close(r)
        os.dup2(w, 1)
        os.execv("/bin/sh", ["/bin/sh", "-c", "zenity --file-selection --file-filter='*.image'"])
    else:
        os.close(w)
        try:
            pid, status = os.waitpid(pid, 0)
        except OSError as e:
            return _Default
        status = os.WEXITSTATUS(status)
        if status == 127:
            return fltk_get_file()
        elif status == 0:
            return os.read(r, 512).strip()
        else:
            return _Default
