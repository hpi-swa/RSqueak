from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.util.cells import QuasiConstant, Cell
from rsqueakvm.plugins.python.py_objspace import new_pypy_objspace
from rsqueakvm.plugins.python.switching import PyFrameRestartInfo
from rsqueakvm.plugins.python.switching import SwitchToSmalltalkAction


py_space = new_pypy_objspace()
switch_action = SwitchToSmalltalkAction(py_space)
py_space.actionflag.register_periodic_action(
    switch_action, use_bytecode_counter=True)

py_frame_restart_info = Cell(None, type=PyFrameRestartInfo)

w_python_resume_method = QuasiConstant(None, type=W_CompiledMethod)
w_python_class = QuasiConstant(None, type=W_PointersObject)
w_python_object_class = QuasiConstant(None, type=W_PointersObject)
w_python_plugin_send = QuasiConstant(None, type=W_PointersObject)


def set_py_frame_restart_info(frame, py_code):
    py_frame_restart_info.set(PyFrameRestartInfo(frame, py_code))
