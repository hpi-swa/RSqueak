from rsqueakvm.model.compiled_methods import (
    W_CompiledMethod, W_SpurCompiledMethod, W_PreSpurCompiledMethod)
from rsqueakvm.model.pointers import W_PointersObject
# from rsqueakvm.plugins.python import execution
from rsqueakvm.plugins.python.py_objspace import new_pypy_objspace
from rsqueakvm.util.cells import QuasiConstant, Cell

from pypy.interpreter.baseobjspace import W_Root as WP_Root

from rpython.rlib import objectmodel


py_space = new_pypy_objspace()
py_globals = py_space.newdict()
py_locals = py_space.newdict()

wp_result = Cell(None, type=WP_Root)
wp_error = Cell(None, type=str)

w_python_resume_method = QuasiConstant(None, type=W_CompiledMethod)
w_python_class = QuasiConstant(None, type=W_PointersObject)
w_python_object_class = QuasiConstant(None, type=W_PointersObject)
w_python_plugin_send = QuasiConstant(None, type=W_PointersObject)

py_runner = Cell(None)

translating = [True]


def startup(space):
    w_python_plugin_send.set(space.wrap_list_unroll_safe([
        space.wrap_string("PythonPlugin"),
        space.wrap_string("send")
    ]))
    w_python_class.set(
        space.smalltalk_at("Python") or space.w_nil.getclass(space)
    )
    w_python_object_class.set(
        space.smalltalk_at("PythonObject") or space.w_nil.getclass(space)
    )
    w_python_resume_method.set(_make_resume_method(space))

    translating[0] = objectmodel.we_are_translated()


def _make_resume_method(space):
    if space.is_spur.is_set():
        w_cm = objectmodel.instantiate(W_SpurCompiledMethod)
    else:
        w_cm = objectmodel.instantiate(W_PreSpurCompiledMethod)
    w_cm.header = 5
    w_cm._primitive = 0
    w_cm.literalsize = 5
    w_cm.islarge = False
    w_cm._tempsize = 0
    w_cm.argsize = 0
    w_cm.compiledin_class = w_python_class.get().getclass(space)
    w_cm.lookup_selector = "fakeResumeFrame"
    w_cm.bytes = [chr(b) for b in [
        0x41,  # pushLit: Processor
        0xD0,  # send: yield
        0x87,  # pop
        0x70,  # pushSelf
        0xD2,  # send: primResume
        0x87,  # pop
        0x78,  # returnSelf
        119,   # method flags and such
        6,
        3,
        255,
    ]]
    from rsqueakvm.wrapper import AssociationWrapper
    w_cm.literals = [
        space.wrap_symbol("yield"),
        space.w_schedulerassociationpointer,
        space.wrap_symbol("primResume"),
        space.wrap_symbol(w_cm.lookup_selector),
        AssociationWrapper.make_w_assoc(
            space,
            space.w_nil,  # wrap_symbol("Python class"),
            w_cm.compiledin_class
        )
    ]
    # import pdb; pdb.set_trace()
    return w_cm
