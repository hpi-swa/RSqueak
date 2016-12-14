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
        ltrace = [lop.name for lop in left.trace]
        rtrace = [rop.name for rop in right.trace]
        import difflib
        answer = (['Comparing Traces failed (set UPDATE_JITTESTS=1 in your env and run again to auto-update):'] +
                  list(difflib.unified_diff(ltrace, rtrace)) +
                  ["-------LEFT------"] +
                  [str(op) for op in left.trace] +
                  ["-------RIGHT-----"] +
                  [str(op) for op in right.trace])
    if os.environ.get("UPDATE_JITTESTS", None) == "1":
        print "\n".join(answer)
        import sys
        sys.stdout.write("Should we accept the new version (y/N)? ")
        if sys.stdin.readline().strip().upper() == "Y":
            import traceback, re
            stk = traceback.extract_stack()
            stk.reverse()
            for filename, lineno, funcname, text in stk:
                if re.search("jittest/test_", filename):
                    f = open(filename)
                    contents = f.readlines()
                    newline = "\r\n" if contents[0].endswith("\r\n") else "\n"
                    while contents[lineno].strip() != str(left.trace[0]).strip():
                        lineno += 1
                    indent = (len(contents[lineno]) - len(contents[lineno].lstrip())) * " "
                    contents[lineno:lineno + len(left.trace)] = [(indent + str(op) + newline) for op in right.trace]
                    f.close()
                    f = open(filename, "w")
                    f.truncate(0)
                    f.writelines(contents)
                    f.close()
                    break
            del sys.exitfunc
            sys.exit(os.system("%s \"%s\"" % (sys.executable, "\" \"".join(sys.argv))))
        else:
            return answer
    else:
        return answer

# Disable image loading output during testing
def fake_update(self, new_steps=-1):
    pass
from rsqueakvm.util.progress import Progress
Progress.update = fake_update
