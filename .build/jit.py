#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# view jit.
#

import sys, os
import environment
try:
    import pygame
except ImportError:
    print "You need PyGame installed"
    exit(1)
if os.system("dot -V") != 0:
    print "You need the dot binary (from Graphviz) installed and in the PATH"
    exit(1)

from rpython import conftest
class o:
    view = False
    viewloops = True
conftest.option = o

from rpython.rlib import jit
from rpython.jit.metainterp.test.test_ajit import LLJitMixin
from rsqueakvm.test.util import import_bytecodes, read_image
from rsqueakvm import model, storage_contexts
import targetrsqueak as rsqueak

sys.setrecursionlimit(10000000)
jit.set_param(None, "trace_limit", 1000000)

import_bytecodes(__name__)
test_jit = LLJitMixin()


def print_result(res):
    if res is not None:
        print "Result: %r" % res

# Pass a function inside here to meta-interpret it and show all encountered loops.
def meta_interp(func):
    return test_jit.meta_interp(func, [], listcomp=True, listops=True, backendopt=True, inline=True)

def load(imagename, space=None):
    _, interp, _, _ = read_image(imagename, space=space)
    return interp

# ==== The following are factories for functions to be passed into meta_interp() ====

def preload_perform(imagename, receiver, selector, *args):
    interp = load(imagename)
    def interp_miniloop():
        return interp.perform(receiver, selector, w_arguments=list(args))
    return interp_miniloop

# This will build a jit executing a synthetic method composed of the given bytecodes and literals,
# and operating on the given stack. The receiver of the 'message' must be at the bottom of the stack.
# The bytecodes can be composed from constants created in this module in above import_bytecodes() call.
def preload_execute_frame(imagename, bytes, literals, stack):
    interp = load(imagename)
    space = interp.space
    w_method = model.W_CompiledMethod(space, header=512)
    w_method.literals = literals
    w_method.setbytes(bytes)
    w_receiver = stack[0]
    s_frame = storage_contexts.ContextPartShadow.build_method_context(space, w_method, w_receiver)
    w_frame = s_frame.w_self()
    def interp_execute_frame():
        return interp.interpret_toplevel(w_frame)
    return interp_execute_frame

# ==== The following will pre-load images and build a jit based on methods from the entry-point module

def run_benchmark(imagename, benchmark, number=0, arg=""):
    interp = load(imagename)
    def interp_run_benchmark():
        return rsqueak._run_benchmark(interp, number, benchmark, arg)
    return interp_run_benchmarks

def run_code(imagename, code, as_benchmark=False):
    from targetrsqueak import prebuilt_space as space, \
            compile_code, create_context, execute_context
    interp = load(image_path(imagename), space=space)
    w_receiver = space.w_nil
    selector = compile_code(interp, w_receiver, code)
    s_frame = create_context(interp, w_receiver, selector, None)
    space.headless.activate()
    context = s_frame
    def interp_run_code():
        w_result = execute_context(interp, context)
        return 0
    return interp_run_code

def run_image(imagename):
    from targetrsqueak import prebuilt_space as space, \
            compile_code, create_context, execute_context, \
            active_context
    interp = load(image_path(imagename), space=space)
    context = active_context(space)
    def interp_run_image():
        w_result = execute_context(interp, context)
        return 0
    return interp_run_image

def image_path(imagename):
    return os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "images", imagename))

# ==== The Main coordinates above methods

def main():
    # ===== First define which image we are going to use.
    # imagename = "minibluebookdebug.image"
    # imagename = "mini.image"
    imagename = "../rsqueakvm/test/images/jittest.image"
    # imagename = "Squeak4.5-13702.image"
    # imagename = "Squeak4.6-vmmaker.1.image"

    # ===== Define the code to be executed, if any.
    # code = "^6+7"
    # code = "10000 timesRepeat: [ 0 makeStackDepth: 10 ]"
    # code = """ 1 to: 100000 do: [:i | (2147483647 bitXor: i) + 10 ]    """
    # code = "10000 timesRepeat: [ (2147483647 bitXor: 12) + 10 ]"
    code = """
        | o |
        o := Array with: (Array with: 'a' with: $b) with: (Array with: 'x' with: $y).
        1 to: 10000 do: [:i | (o at: (i \\\\ 2) + 1) at: (i \\\\ 2) + 1].
    """
    # code = "1 to: 10000 do: [:i | (2147483647 bitXorLarge: i) + 10 ]"
    # code = "1 to: 10000 do: [:i | i ]"

    # ===== These entry-points pre-load the image and directly execute a single frame.
    # func = preload_perform(imagename, model.W_SmallInteger(1000), 'loopTest2')
    # func = preload_perform(imagename, model.W_SmallInteger(777), 'name')
    # func = preload_execute_frame(imagename, [returnReceiverBytecodeBytecode], [], [model.W_SmallInteger(42)])

    # ==== These entry-points pre-load the image and then use methods from the entry-point module.
    # ==== This is very close to what actually happens in the VM, but with a pre-loaded image.
    # func = run_benchmark(imagename, "loopTest2", 10000)
    func = run_code(imagename, code, as_benchmark=False)
    # func = run_image(imagename)

    # ===== Now we can either simply execute the entry-point, or meta-interpret it (showing all encountered loops).
    # res = func()
    res = meta_interp(func)
    print_result(res)

# This is for execution using pytest.py. This way you can get a pdb on assertion-errors etc.
# Execute e.g. $ pypy ../pypy/pytest.py rsqueakvm/test/jit.py -s --pdb -k test_main
def test_main():
    main()

if __name__ == "__main__":
    main()
