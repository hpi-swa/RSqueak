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


from spyvm import model, interpreter, primitives, shadow
from spyvm import objspace
from spyvm.tool.analyseimage import create_squeakimage, create_testimage


mockclass = objspace.bootstrap_class

space = objspace.ObjSpace()

# expose the bytecode's values as global constants.
# Bytecodes that have a whole range are exposed as global functions:
# call them with an argument 'n' to get the bytecode number 'base + n'.
# XXX hackish
def setup():
    def make_getter(entry):
        def get_opcode_chr(n):
            opcode = entry[0] + n
            assert entry[0] <= opcode <= entry[1]
            return chr(opcode)
        return get_opcode_chr
    for entry in interpreter.BYTECODE_RANGES:
        name = entry[-1]
        if len(entry) == 2:     # no range
            globals()[name] = chr(entry[0])
        else:
            globals()[name] = make_getter(entry)
setup()

#
# Tests
#

sys.setrecursionlimit(100000)

class TestLLtype(LLJitMixin):

    def test_miniloop(self):

        from spyvm import objspace
        space = objspace.ObjSpace()
        image = create_testimage(space)
        interp = interpreter.Interpreter(space, image)


        counter = 0

        def interp_w():

            w_object = model.W_SmallInteger(0)
            interp.perform(w_object, "loopTest")

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)
