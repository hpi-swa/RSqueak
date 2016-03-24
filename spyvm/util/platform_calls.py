from rpython.translator.tool.cbuild import ExternalCompilationInfo
from rpython.translator.platform import CompilationError
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rlib.rarithmetic import intmask
from rpython.rtyper.tool import rffi_platform as platform
import py
import sys
import os
from spyvm.util import system

if system.IS_WINDOWS:
    libraries = ["psapi.lib"]
else:
    libraries = []

this_dir = os.path.dirname(__file__)
eci = ExternalCompilationInfo(
    post_include_bits = ["""
#ifndef __platform_h
#define __platform_h

#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
#define DLLEXPORT __declspec(dllexport)
#else
#include <sys/time.h>
#include <sys/resource.h>
#define DLLEXPORT __attribute__((__visibility__("default")))
#endif

#ifdef __cplusplus
extern "C" {
#endif
        DLLEXPORT int RSqueakGetMemoryUsage();
#ifdef __cplusplus
}
#endif

#endif"""],
    include_dirs = [this_dir],
    link_files = libraries,
    separate_module_sources = ["""
int RSqueakGetMemoryUsage() {
#ifdef _WIN32
        PROCESS_MEMORY_COUNTERS_EX memCountr;
        if (GetProcessMemoryInfo(GetCurrentProcess(), &memCountr, sizeof(memCountr))) {
            return (int)(memCountr.PrivateUsage);
        } else {
            return -1;
        }
#else
        struct rusage usage;
        if (!getrusage(RUSAGE_SELF, &usage)) {
            return (int)(usage.ru_maxrss * 1024);
        } else {
            return -1;
        }
#endif
}"""]
)

__ll_memory_usage = rffi.llexternal('RSqueakGetMemoryUsage', [], rffi.INT, compilation_info=eci)
def get_memory_usage():
    res = __ll_memory_usage()
    return intmask(res)
