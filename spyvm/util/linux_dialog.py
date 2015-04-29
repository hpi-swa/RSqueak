from rpython.translator.tool.cbuild import ExternalCompilationInfo
from rpython.translator.platform import CompilationError
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rtyper.tool import rffi_platform as platform
import py
import os, sys
from spyvm.util import system

assert system.IS_LINUX

this_dir = py.path.local(__file__).dirpath()

###############################################################################

import subprocess
if subprocess.call("fltk-config --version", shell=True) == 0:
    cflags = subprocess.check_output("fltk-config --cxxflags", shell=True).strip().split()
    ldflags = subprocess.check_output("fltk-config --ldstaticflags", shell=True).strip().split()
    eci = ExternalCompilationInfo(
        includes = [str(this_dir.join('linux/file_chooser.h'))],
        link_files = [str(this_dir.join('linux/file_chooser.cxx'))],
        libraries = ["stdc++"],
        compile_extra = cflags,
        link_extra = ldflags,
        separate_module_sources = ["""
        int irrelevant_caller_of_target_function(char* buffer, int len)
        {
            return RSqueakOpenFileDialog_linux(buffer, len);
        }
        """]
    )

    __llget_file = rffi.llexternal('RSqueakOpenFileDialog_linux', [rffi.CCHARP, rffi.INT], rffi.INT, compilation_info=eci)
    def get_file():
        charp = rffi.str2charp("".join(["\0"] * 260))
        res = __llget_file(charp, 260)
        if (res == 1):
            path = rffi.charp2str(charp)
            rffi.free_charp(charp)
            return path
        else:
            return "Squeak.image"
else:
    print "\033[33;5;7m You do not have FLTK installed - file picker will fall back to Zenity at runtime \033[0m"
    def get_file():
        default = "Squeak.image"
        r, w = os.pipe()
        pid = os.fork()
        if pid == 0:
            os.close(r)
            os.dup2(w, 1)
            os.execv("/bin/sh", ["/bin/sh", "-c", "zenity2 --file-selection --file-filter='*.image'"])
        else:
            os.close(w)
            try:
                pid, status = os.waitpid(pid, 0)
            except OSError as e:
                return default
            status = os.WEXITSTATUS(status)
            if status != 0:
                if status == 127:
                    print "\033[33;5;7m You do not have Zenity installed - file picker not working \033[0m"
                return default
            else:
                return os.read(r, 512).replace("\n", "")
