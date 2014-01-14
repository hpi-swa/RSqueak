import subprocess
import os

# TODO:
from pypy.tool.jitlogparser.parser import SimpleParser, Op
from pypy.tool.jitlogparser.storage import LoopStorage

from rpython.jit.metainterp.resoperation import opname
from rpython.jit.tool import oparser
from rpython.tool import logparser


BasePath = os.path.abspath(
    os.path.join(
        os.path.join(os.path.dirname(__file__), os.path.pardir),
        os.path.pardir,
        os.path.pardir
    )
)
BenchmarkImage = os.path.join(os.path.dirname(__file__), "benchmark.image")

class BaseJITTest(object):
    def run(self, spy, tmpdir, code):
        proc = subprocess.Popen(
            [str(spy), "-r", code.replace("\n", "\r\n"), BenchmarkImage],
            cwd=str(tmpdir),
            env={"PYPYLOG": "jit-log-opt:%s" % tmpdir.join("x.pypylog")}
        )
        proc.wait()
        data = logparser.parse_log_file(str(tmpdir.join("x.pypylog")), verbose=False)
        data = logparser.extract_category(data, "jit-log-opt-")

        storage = LoopStorage()
        traces = [SimpleParser.parse_from_input(t) for t in data]
        main_loops = storage.reconnect_loops(traces)
        traces_w = []
        for trace in traces:
            if trace in main_loops:
                traces_w.append(Trace(trace))
            else:
                traces_w[len(traces_w) - 1].addbridge(trace)
        return traces_w

    def assert_matches(self, trace, expected):
        expected_lines = [
            line.strip()
            for line in expected.splitlines()
            if line and not line.isspace()
        ]
        parser = Parser(None, None, {}, "lltype", None, invent_fail_descr=None, nonstrict=True)
        expected_ops = [parser.parse_next_op(l) for l in expected_lines]
        aliases = {}
        assert len(trace) == len(expected_ops)
        for op, expected in zip(trace, expected_ops):
            self._assert_ops_equal(aliases, op, expected)

    def _assert_ops_equal(self, aliases, op, expected):
        assert op.name == expected.name
        assert len(op.args) == len(expected.args)
        for arg, expected_arg in zip(op.args, expected.args):
            if arg in aliases:
                arg = aliases[arg]
            elif arg != expected_arg and expected_arg not in aliases.viewvalues():
                aliases[arg] = arg = expected_arg
            assert arg == expected_arg


class Parser(oparser.OpParser):
    def get_descr(self, poss_descr, allow_invent):
        if poss_descr.startswith(("TargetToken", "<Guard")):
            return poss_descr
        return super(Parser, self).get_descr(poss_descr, allow_invent)

    def getvar(self, arg):
        return arg

    def create_op(self, opnum, args, res, descr):
        return Op(opname[opnum].lower(), args, res, descr)


class Trace(object):
    def __init__(self, trace):
        self._trace = trace
        self._bridges = []
        self._bridgeops = None
        self._loop = None

    def addbridge(self, trace):
        self._bridges.append(trace)

    @property
    def bridges(self):
        if self._bridgeops:
            return self._bridgeops
        else:
            self._bridgeops = []
            for bridge in self._bridges:
                self._bridgeops.append([op for op in bridge.operations if not op.name.startswith("debug_")])
            return self._bridgeops

    @property
    def loop(self):
        if self._loop:
            return self._loop
        else:
            self._loop = self._parse_loop_from(self._trace)
            return self._loop

    def _parse_loop_from(self, trace, label_seen=None):
        _loop = []
        for idx, op in enumerate(self._trace.operations):
            if label_seen and not op.name.startswith("debug_"):
                _loop.append(op)
            if op.name == "label":
                if label_seen is None: # first label
                    label_seen = False
                else:
                    label_seen = True # second label
        if len(_loop) == 0:
            raise ValueError("Loop body couldn't be found")
        return _loop
