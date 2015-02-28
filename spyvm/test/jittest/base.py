import subprocess, os
from rpython.tool.jitlogparser.parser import Op
from rpython.jit.metainterp.resoperation import opname
from rpython.jit.tool import oparser
from spyvm.util import logparser
from spyvm.test.util import image_path

TestImage = image_path("jittest.image")

class BaseJITTest(object):
    def run(self, spy, tmpdir, code):
        logfile = str(tmpdir.join("x.pypylog"))
        print logfile
        proc = subprocess.Popen(
            [str(spy), TestImage, "-r", code.replace("\n", "\r\n")],
            cwd=str(tmpdir),
            env={"PYPYLOG": "jit-log-opt:%s" % logfile,
                 "SDL_VIDEODRIVER": "dummy"}
        )
        proc.wait()
        return logparser.extract_traces(logfile)

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
        # assert len(op.args) == len(expected.args)
        # for arg, expected_arg in zip(op.args, expected.args):
        #     if arg in aliases:
        #         arg = aliases[arg]
        #     elif arg != expected_arg and expected_arg not in aliases.viewvalues():
        #         aliases[arg] = arg = expected_arg
        #     assert arg == expected_arg

class Parser(oparser.OpParser):
    def get_descr(self, poss_descr, allow_invent):
        if poss_descr.startswith(("TargetToken", "<Guard")):
            return poss_descr
        return super(Parser, self).get_descr(poss_descr, allow_invent)

    def getvar(self, arg):
        return arg

    def create_op(self, opnum, args, res, descr, fail_args):
        return Op(opname[opnum].lower(), args, res, descr)
