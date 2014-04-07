#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# view jit.
#

import sys
from rpython import conftest
class o:
    view = False
    viewloops = True
conftest.option = o

from rpython.jit.metainterp.test.test_ajit import LLJitMixin
from spyvm.test.util import import_bytecodes, read_image
from spyvm import model, shadow

sys.setrecursionlimit(5000)
import_bytecodes(__name__)
jit = LLJitMixin()

def print_result(res):
    if res is not None:
        print "Result: %r" % res

# Pass a function inside here to meta-interpret it and show all encountered loops.
def meta_interp(func):
    return jit.meta_interp(func, [], listcomp=True, listops=True, backendopt=True, inline=True)

# ==== The following are factories for functions to be passed into meta_interp() ====

# This will build a small jit just for the specified message-send
def perform(imagename, receiver, selector, *args):
    _, interp, _, _ = read_image(imagename)
    def interp_miniloop():
        return interp.perform(receiver, selector, *args)
    return interp_miniloop

# This will build a jit executing a synthetic method composed of the given bytecodes and literals,
# and operating on the given stack. The receiver of the 'message' must be at the bottom of the stack.
# The bytecodes can be composed from constants created in this module in above import_bytecodes() call.
def execute_frame(imagename, bytes, literals, stack):
    space, interp, _, _ = read_image(imagename)
    w_method = model.W_CompiledMethod(space, header=512)
    w_method.literals = literals
    w_method.setbytes(bytes)
    w_receiver = stack[0]
    s_frame = shadow.MethodContextShadow(space, None, w_method, w_receiver, [])
    w_frame = s_frame.w_self()
    def interp_execute_frame():
        return interp.loop(w_frame)
    return interp_execute_frame

# This will build a JIT for the entire VM. Arguments to the VM entry-point must be provided.
def full_vm(args):
    import targetimageloadingsmalltalk
    module_file = targetimageloadingsmalltalk.__file__[:-1]
    full_args = [ module_file ]
    full_args.extend([ str(a) for a in args ])
    print ">> Entry Point arguments: %r" % full_args
    def interp_full_vm():
        return targetimageloadingsmalltalk.entry_point(full_args)
    return interp_full_vm

def open_image(imagename, additional_args = []):
    args = ["images/" + imagename]
    args.extend(additional_args)
    return full_vm(args)

def run_vm_code(imagename, code):
    return open_image(imagename, ['-r', code])
    
def execute_vm_method(imagename, selector, receiver_num=None, string_arg=None):
    args = ['-m', selector]
    if string_arg:
        args.extend(['-a', string_arg])
    if receiver_num:
        args.extend(['-n', receiver_num])
    return open_image(imagename, args)

def main():
    # ===== First define which image we are going to use.
    imagename = "mini.image"
    # imagename = "minitest.image"
    # imagename = "Squeak4.5-noBitBlt.image"
    
    # ===== These entry-points pre-load the image and execute just a single frame.
    # func = perform(imagename, model.W_SmallInteger(1000), 'loopTest2')
    func = perform(imagename, model.W_SmallInteger(777), 'name')
    # func = execute_frame(imagename, [returnReceiver], [], [model.W_SmallInteger(42)])
    
    # ===== These execute the complete interpreter
    # XXX These do not work because loading the image file while meta-interpreting always leads to 
    # a 'Bad file descriptor' error.
    # func = execute_vm_method("mini.image", "name", 33)
    # func = run_vm_code(imagename, "^5+6")
    # func = execute_vm_method(imagename, "name", 33)
    # func = open_image(imagename)
    
    # ===== Now we can either simply execute the entry-point, or meta-interpret it (showing all encountered loops).
    # import pdb; pdb.set_trace()
    # res = func()
    res = meta_interp(func)
    print_result(res)

# This is for execution using pytest.py. This way you can get a pdb on assertion-errors etc.
# Execute e.g. $ pypy ../pypy/pytest.py spyvm/test/jit.py -s --pdb -k test_main
def test_main():
    main()

if __name__ == "__main__":
    main()
