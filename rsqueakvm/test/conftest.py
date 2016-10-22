import py, os, subprocess

def pytest_addoption(parser):
    group = parser.getgroup("RSqueak test options")
    group.addoption(
        "--quick", "-Q",
        dest="execute-quick-tests",
        action="store_false",
        default=True,
        help="Only execute quick tests (no image loading or long execution)"
    )
    group.addoption(
        "--slow", "-S",
        dest="execute-slow-tests",
        action="store_true",
        default=False,
        help="Execute all tests (including very slow tests)"
    )
    group.addoption(
        "--jit",
        dest="rsqueak-binary",
        action="store",
        default=None,
        help="Path to a compiled rsqueak binary. Enables jit tests."
    )

    group.addoption(
        "--jitargs",
        dest="jitargs",
        action="store",
        default=None,
        help="Arguments passed through to rsqueak."
    )

class _Executor_(object):
    def __init__(self, executable, *args):
        self.executable = executable
        self.args = list(args)
    def all_arguments(self, args):
        return map(str, [self.executable] + self.args + args)
    def system(self, *args):
        return os.system(" ".join(self.all_arguments(list(args))))
    def popen(self, *args, **kwargs):
        return subprocess.Popen(self.all_arguments(list(args)), **kwargs)

# The 'jit' parameter is used in tests under jittest/
def pytest_funcarg__spy(request):
    val = request.config.getvalue("rsqueak-binary")
    if not val:
        py.test.skip("Provide --jit parameter to execute jit tests")
    jitarg = request.config.getvalue("jitargs")
    if not jitarg:
        return _Executor_(py.path.local(val))
    else:
        return _Executor_(py.path.local(val), jitarg)

from .jittest.base import Trace
def pytest_assertrepr_compare(op, left, right):
    if isinstance(left, Trace) and isinstance(right, Trace) and op == "==":
        return (['Comparing Traces failed:'] +
                [str(op) for op in left.trace] +
                ["-----------------"] +
                [str(op) for op in right.trace])


# Disable image loading output during testing
def fake_update(self, new_steps=-1):
    pass
from rsqueakvm.util.progress import Progress
Progress.update = fake_update
