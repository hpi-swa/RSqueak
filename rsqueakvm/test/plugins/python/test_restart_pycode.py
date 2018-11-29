import sys
import dis
import StringIO

from rsqueakvm.plugins.python.patching import patch_pypy
from rsqueakvm.plugins.python.utils import get_restart_pycode


patch_pypy()


def disassembly_test(result, expected):
    s = StringIO.StringIO()
    save_stdout = sys.stdout
    sys.stdout = s
    dis.dis(result._to_code())
    sys.stdout = save_stdout
    got = s.getvalue()
    lines = got.split('\n')
    expected = expected.split('\n')
    lines = [line.rstrip() for line in lines]
    # import pdb; pdb.set_trace()
    import difflib
    if expected != lines:
        assert False, 'events did not match expectation:\n' + \
                      '\n'.join(difflib.ndiff(expected, lines))


def test_simple_eval():
    result = get_restart_pycode('1', cmd='eval')
    disassembly_test(result, """\
  1           0 LOAD_CONST               0 (1)
              3 RETURN_VALUE
""")


def test_simple_exec():
    result = get_restart_pycode('import sys', cmd='exec')
    disassembly_test(result, """\
  1           0 LOAD_CONST               0 (-1)
              3 LOAD_CONST               1 (None)
              6 IMPORT_NAME              0 (sys)
              9 STORE_NAME               0 (sys)
             12 LOAD_CONST               1 (None)
             15 RETURN_VALUE
""")


def test_nested_exec():
    result = get_restart_pycode('def func():\n  return 42', cmd='exec')
    disassembly_test(result, """\
  2           0 LOAD_CONST               1 (42)
              3 RETURN_VALUE
""")


def test_invalid_eval():
    result = get_restart_pycode('import os', cmd='eval')
    assert result is None


def test_invalid_exec():
    result = get_restart_pycode('in valid syntax')
    assert result is None
