import py

option = None

def pytest_configure(config):
    global option
    option = config.option

def pytest_addoption(parser):
    group = parser.getgroup("smalltalk options")
    group.addoption('--bc-trace',
               action="store_true",
               dest="bc_trace",
               default=False,
               help="print bytecodes and stack during execution")
    group.addoption('--prim-trace',
               action="store_true",
               dest="prim_trace",
               default=False,
               help="print called primitives during execution")
