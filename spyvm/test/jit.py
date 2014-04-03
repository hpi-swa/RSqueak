#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# view jit.
#

import sys, os
from rpython import conftest
class o:
    view = False
    viewloops = True
conftest.option = o

from rpython.jit.metainterp.test.test_ajit import LLJitMixin

from .util import import_bytecodes
from spyvm import model, interpreter, primitives, shadow
from spyvm import objspace, squeakimage
from spyvm.tool.analyseimage import create_squeakimage, create_testimage
from rpython.rlib.streamio import open_file_as_stream

import_bytecodes(__name__)

space = objspace.ObjSpace()

sys.setrecursionlimit(5000)

class TestLLtype(LLJitMixin):

    def test_miniloop(self):

        from spyvm import objspace
        space = objspace.ObjSpace()

        image = create_testimage(space)
        interp = interpreter.Interpreter(space, image)
        w_selector = interp.perform(space.wrap_string('loopTest2'), "asSymbol")
        assert isinstance(w_selector, model.W_BytesObject)
        def interp_w():
            interp.perform(model.W_SmallInteger(1000), w_selector)

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True, inline=True)
