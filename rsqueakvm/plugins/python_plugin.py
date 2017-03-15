from rsqueakvm.plugins.python import PythonPlugin

try:
    from rsqueakvm.plugins.python import utils
    from rsqueakvm.plugins.python.model import W_PythonObject
except ImportError:
    pass

plugin = PythonPlugin()


@plugin.expose_primitive(unwrap_spec=[object, object, str, str, str])
def restartSpecificFrame(interp, s_frame, w_rcvr, w_frame, source, filename,
                         cmd):
    frame = None
    if isinstance(w_frame, W_PythonObject):
        frame = w_frame.wp_object
    py_code = None
    if source:
        py_code = utils.get_restart_pycode(source, filename, cmd)
        if py_code is None:
            return interp.space.w_false  # Raising prim error causes crashes
    PythonPlugin.set_py_frame_restart_info(frame, py_code)
    return interp.space.w_true
