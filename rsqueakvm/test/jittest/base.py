import os, shutil
from rpython.tool.jitlogparser.parser import Op
from rpython.jit.metainterp.resoperation import opname
from rpython.jit.tool import oparser
from rsqueakvm.util import logparser
from rsqueakvm.test.util import image_path


aliases = {
    "guard_class": "guard_nonnull_class",
}
for k, v in aliases.items():
    aliases[v] = k


class Trace(object):
    def __init__(self, string=None, trace=None):
        assert string or trace
        self.trace = trace
        self.string = string

    def __eq__(self, other):
        if not isinstance(other, Trace): return False
        try:
            self._parse()
        except Exception, e:
            self.trace = [str(e)]
            return False
        try:
            other._parse()
        except Exception, e:
            other.trace = [str(e)]
            return False
        for op, expected in zip(self.trace, other.trace):
            if not self._ops_equal(op, expected):
                return False
        return True

    def _parse(self):
        if not self.trace:
            parser = Parser(None, None, {}, "lltype", invent_fail_descr=None, nonstrict=True)
            self.trace = [parser.parse_next_op(l) for l in self.string]

    def _ops_equal(self, op, expected):
        return op.name == expected.name or aliases.get(op.name, None) == expected.name


class BaseJITTest(object):
    test_image = image_path("jittest.image")

    def run(self, spy, tmpdir, code):
        logfile = str(tmpdir.join("x.pypylog"))
        print logfile
        proc = spy.popen(
            "--reader-jit-args", "off", "-r", code.replace("\n", "\r\n"), self.test_image,
            cwd=str(tmpdir),
            env={"PYPYLOG": "jit-log-opt:%s" % logfile,
                 "SDL_VIDEODRIVER": "dummy"}
        )
        proc.wait()
        return logparser.extract_traces(logfile)

    def assert_matches(self, trace, expected_str):
        expected_lines = [
            line.strip()
            for line in expected_str.splitlines()
            if line and not line.isspace()
        ]
        assert Trace(string=expected_lines) == Trace(trace=trace)


class ModernJITTest(BaseJITTest):
    image_name = "Squeak4.3.image"
    test_image = image_path(image_name)

    def run(self, spy, tmpdir, code):
        # first loop is from compiling
        return BaseJITTest.run(self, spy, tmpdir, code)[1:]

class Parser(oparser.OpParser):
    def get_descr(self, poss_descr, allow_invent):
        if poss_descr.startswith(("TargetToken", "<Guard")):
            return poss_descr
        return super(Parser, self).get_descr(poss_descr, allow_invent)

    def getvar(self, arg):
        return arg

    def update_vector(self, resop, res):
        return res

    def create_op(self, opnum, args, res, descr, fail_args):
        return Op(opname[opnum].lower(), args, res, descr)
