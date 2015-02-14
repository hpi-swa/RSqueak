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
from spyvm import model, storage_contexts

sys.setrecursionlimit(5000)
import_bytecodes(__name__)
jit = LLJitMixin()

def print_result(res):
    if res is not None:
        print "Result: %r" % res

# Pass a function inside here to meta-interpret it and show all encountered loops.
def meta_interp(func):
    return jit.meta_interp(func, [], listcomp=True, listops=True, backendopt=True, inline=True)

def load(imagename):
    _, interp, _, _ = read_image(imagename)
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
    import targetimageloadingsmalltalk
    interp = load(imagename)
    def interp_run_benchmark():
        return targetimageloadingsmalltalk._run_benchmark(interp, number, benchmark, arg)
    return interp_run_benchmarks

def run_code(imagename, code, as_benchmark=False):
    from targetimageloadingsmalltalk import prebuilt_space as space, \
            compile_code, create_context, execute_context
    interp = load(imagename)
    def interp_run_code():
        w_receiver = space.w_nil
        selector = compile_code(interp, w_receiver, code)
        s_frame = create_context(interp, w_receiver, selector, None)
        space.headless.activate()
        context = s_frame
        w_result = execute_context(interp, context)
        return 0
    return interp_run_code

def run_image(imagename):
    import targetimageloadingsmalltalk
    interp = load(imagename)
    def interp_run_image():
        return targetimageloadingsmalltalk._run_image(interp)
    return interp_run_image

# ==== The following will build a JIT for the real entry-point.

def full_vm(args):
    import targetimageloadingsmalltalk
    module_file = targetimageloadingsmalltalk.__file__[:-1]
    full_args = [ module_file ]
    full_args.extend([ str(a) for a in args ])
    print ">> Entry Point arguments: %r" % full_args
    def interp_full_vm():
        return targetimageloadingsmalltalk.entry_point(full_args)
    return interp_full_vm

def full_vm_image(imagename, additional_args = []):
    args = ["images/" + imagename]
    args.extend(additional_args)
    return full_vm(args)

def full_vm_code(imagename, code):
    return full_vm_image(imagename, ['-r', code])

def full_vm_method(imagename, selector, receiver_num=None, string_arg=None):
    args = ['-m', selector]
    if string_arg:
        args.extend(['-a', string_arg])
    if receiver_num:
        args.extend(['-n', receiver_num])
    return full_vm_image(imagename, args)

# ==== The Main coordinates above methods

def main():
    # ===== First define which image we are going to use.
    # imagename = "minibluebookdebug.image"
    imagename = "mini.image"
    # imagename = "Squeak4.5-noBitBlt.image"

    # ===== Define the code to be executed, if any.
    # code = "^6+7"
    code = "10000 timesRepeat: [ 0 makeStackDepth: 10 ]"

    # ===== These entry-points pre-load the image and directly execute a single frame.
    # func = preload_perform(imagename, model.W_SmallInteger(1000), 'loopTest2')
    # func = preload_perform(imagename, model.W_SmallInteger(777), 'name')
    # func = preload_execute_frame(imagename, [returnReceiverBytecodeBytecode], [], [model.W_SmallInteger(42)])

    # ===== These execute the complete interpreter
    # ===== XXX These do not work because loading the image file while meta-interpreting always leads to
    # ===== a 'Bad file descriptor' error.
    # func = full_vm_code(imagename, code)
    # func = full_vm_method(imagename, "name", 33)
    # func = full_vm_image(imagename)

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
# Execute e.g. $ pypy ../pypy/pytest.py spyvm/test/jit.py -s --pdb -k test_main
def test_main():
    main()

if __name__ == "__main__":
    main()
