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

imagename = "mini.image"
# imagename = "minitest.image"

sys.setrecursionlimit(5000)
import_bytecodes(__name__)
jit = LLJitMixin()

# Pass a function inside here to meta-interpret it and show all encountered loops.
def meta_interp(func):
    res = jit.meta_interp(func, [], listcomp=True, listops=True, backendopt=True, inline=True)
    if res:
        print res.__repr__()

# ==== The following are factories for functions to be passed into meta_interp() ====

# This will build a small jit just for the specified message-send
def perform(receiver, selector, *args):
    _, interp, _, _ = read_image(imagename)
    def interp_miniloop():
        interp.perform(receiver, selector, *args)
    return interp_miniloop

# This will build a jit executing a synthetic method composed of the given bytecodes and literals,
# and operating on the given stack. The receiver of the 'message' must be at the bottom of the stack.
# The bytecodes can be composed from constants created in this module in above import_bytecodes() call.
def execute_frame(bytes, literals, stack):
    space, interp, _, _ = read_image(imagename)
    w_method = model.W_CompiledMethod(space, header=512)
    w_method.literals = literals
    w_method.setbytes(bytes)
    w_receiver = stack[0]
    s_frame = shadow.MethodContextShadow(space, None, w_method, w_receiver, [])
    w_frame = s_frame.w_self()
    def interp_execute_bytes_with_stack():
        interp.loop(w_frame)
    return interp_execute_bytes_with_stack

# This will build a JIT for the entire VM.
def full_vm():
    import targetimageloadingsmalltalk
    argv = sys.argv
    def interp_full_vm():
        targetimageloadingsmalltalk.entry_point(argv)
    return interp_full_vm

def main():
    # func = perform(model.W_SmallInteger(1000), 'loopTest2')
    # func = perform(model.W_SmallInteger(777), 'name')
    func = execute_frame([returnReceiver], [], [model.W_SmallInteger(42)])
    # func = full_vm()
    meta_interp(func)

# This is for execution using pytest.py. This way you can get a pdb on assertion-errors etc.
# Execute e.g. $ pypy ../pypy/pytest.py spyvm/test/jit.py -s --pdb -k test_main
def test_main():
    main()

if __name__ == "__main__":
    main()
