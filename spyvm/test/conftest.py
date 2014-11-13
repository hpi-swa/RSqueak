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

# The 'jit' parameter is used in tests under jittest/
def pytest_funcarg__spy(request):
    val = request.config.getvalue("rsqueak-binary")
    if not val:
        py.test.skip("Provide --jit parameter to execute jit tests")
    return str(py.path.local(val))
