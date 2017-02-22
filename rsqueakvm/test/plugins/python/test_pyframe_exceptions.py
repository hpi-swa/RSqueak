from rsqueakvm.plugins.python.global_state import py_space
from rsqueakvm.plugins.python.patching import patch_pypy

from pypy.interpreter.error import OperationError
from pypy.interpreter.main import compilecode

patch_pypy()


def check_for_exception_handler(code):
    pycode = compilecode(py_space, code, '<string>', 'exec')
    py_frame = py_space.FrameClass(py_space, pycode, py_space.newdict(), None)
    try:
        py_frame.dispatch_bytecode(pycode.co_code, 0,
                                   py_space.getexecutioncontext())
    except OperationError as operr:
        return py_frame.has_exception_handler(operr)


def test_simple_exception():
    assert check_for_exception_handler("""
try:
    1/0
except ZeroDivisionError:
    pass
""")


def test_simple_fail_exception():
    assert not check_for_exception_handler("""1/0""")


def test_raised_exception():
    assert check_for_exception_handler("""
try:
    raise Exception
except Exception:
    pass
""")


def test_multiple_exceptions():
    assert check_for_exception_handler("""
try:
    list()[1]
except (ValueError, IndexError):
    pass
""")


def test_multiple_exceptions_fail():
    assert not check_for_exception_handler("""
try:
    list()[1]
except (ValueError, ZeroDivisionError):
    pass
""")


def test_catch_all_exceptions():
    assert check_for_exception_handler("""
try:
    1/0
except:
    pass
""")


def test_catch_variable_exception():
    assert check_for_exception_handler("""
ex = IndexError
if True:
    ex = ZeroDivisionError
try:
    1/0
except ex:
    pass
""")


def test_catch_variable_exception_fail():
    assert not check_for_exception_handler("""
ex = ZeroDivisionError
if True:
    ex = IndexError
try:
    1/0
except ex:
    pass
""")


def test_catch_multiple_variable_exceptions():
    assert check_for_exception_handler("""
ex = (ValueError, ZeroDivisionError)
try:
    1/0
except ex:
    pass
""")


def test_catch_nested_exceptions():
    assert check_for_exception_handler("""
try:
    try:
        1/0
    except ValueError:
        pass
except ZeroDivisionError:
    pass
""")


def test_catch_nested_exceptions_fail():
    assert not check_for_exception_handler("""
try:
    try:
        1/0
    except ValueError:
        pass
except IndexError:
    pass
""")
