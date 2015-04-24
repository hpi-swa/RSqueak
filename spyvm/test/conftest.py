import py

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
        "--squeak",
        dest="squeakbinary",
        action="store",
        default=None,
        help="Path to a Squeak binary (Cog or interpreter). Enables jit tests."
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
        import os
        return os.system(" ".join(self.all_arguments(list(args))))
    def popen(self, *args, **kwargs):
        import subprocess
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

def pytest_funcarg__squeak(request):
    val = request.config.getvalue("squeakbinary")
    if not val:
        py.test.skip("Provide --squeak parameter to execute modern jit tests")
    return _Executor_(py.path.local(val))
