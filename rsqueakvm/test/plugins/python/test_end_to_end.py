from rsqueakvm.model.base import W_Object
from rsqueakvm.model.numeric import W_SmallInteger
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.plugins.python import PythonPlugin
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.objspace import py_space
from rsqueakvm.plugins.python.patching import patch_pypy
from rsqueakvm.test.util import create_space, cleanup_module, read_image


def test_space():
    return create_space(bootstrap=True)

patch_pypy()
space = interp = perform = w = None
NAME_TEMPLATE = 'getPy%s'
CALL_COUNTER = 0


def get_next_name():
    global CALL_COUNTER
    CALL_COUNTER += 1
    return NAME_TEMPLATE % CALL_COUNTER


def perform(receiver, selector, *args):
        w_selector = None if isinstance(selector, str) else selector
        return interp.perform(receiver, selector, w_selector, list(args))


def setup_module():
    global space, interp, perform, w
    space, interp, image, reader = read_image('pypy.image')
    w = space.w
    space.runtime_setup(interp, '', [], '', 0)
    space.headless.activate()
    PythonPlugin.startup(space, [])


def teardown_module():
    cleanup_module(__name__)


def add_method_and_call(sourcecode, name):
    perform(w(0).getclass(space), 'compile:classified:notifying:',
            w(sourcecode), w('pypy'), w(None))
    return perform(w(0), name)


def get_python_result(code, cmd='eval'):
    name = get_next_name()
    sourcecode = """%s
        ^ Python %s: '%s'""" % (name, cmd, code)
    w_res = add_method_and_call(sourcecode, name)
    assert isinstance(w_res, W_PythonObject)
    return py_space.unwrap(w_res.wp_object)


def get_smalltalk_result(code, cmd='eval'):
    name = get_next_name()
    sourcecode = """%s
        ^ (Python %s: '%s') asSmalltalk""" % (name, cmd, code)
    w_res = add_method_and_call(sourcecode, name)
    assert isinstance(w_res, W_Object)
    return w_res


def test_simple():
    assert get_python_result('None') is None
    assert get_python_result('1 + 1') == 2
    assert get_python_result('dict(a=2, b=True)') == {'a': 2, 'b': True}


def test_assignment():
    assert get_python_result('foo = 42', 'exec') is None
    assert get_python_result('foo', 'eval') == 42


def test_smalltalk():
    res = get_smalltalk_result('21 * 2')
    assert isinstance(res, W_SmallInteger)
    assert res.value == 42

    res = get_smalltalk_result('[1, 2, 3, 4]')
    assert isinstance(res, W_PointersObject)
    assert res.getclass(space) is space.w_Array
    assert res.size() == 4


def test_error():
    x = get_python_result('1/0', 'eval')
    assert len(x) == 2
    assert x[0] == 'ZeroDivisionError'
