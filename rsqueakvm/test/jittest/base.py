import os, shutil
from rpython.tool.jitlogparser.parser import Op
from rpython.jit.metainterp.resoperation import opname
from rpython.jit.tool import oparser
from rsqueakvm.util import logparser
from rsqueakvm.test.util import image_path

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

    def assert_matches(self, trace, expected):
        expected_lines = [
            line.strip()
            for line in expected.splitlines()
            if line and not line.isspace()
        ]
        parser = Parser(None, None, {}, "lltype", invent_fail_descr=None, nonstrict=True)
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


class ModernJITTest(BaseJITTest):
    image_name = "Squeak4.3.image"
    test_image = image_path(image_name)

    def prepare(self, squeak, tmpdir, code):
        self.has_copied = True
        shutil.copyfile(self.test_image, str(tmpdir.join(self.image_name)))
        infile = tmpdir.join("input.st")
        f = open(str(infile), 'w')
        f.write("Utilities setAuthorInitials: 'foo'. %s Smalltalk snapshot: true andQuit: true." % code)
        f.close()
        curdir = os.getcwd()
        os.chdir(str(tmpdir))
        try:
            squeak.system(self.image_name, infile)
        finally:
            os.chdir(curdir)

    def run(self, spy, squeak, tmpdir, code):
        if not getattr(self, "has_copied", False):
            shutil.copyfile(self.test_image, str(tmpdir.join(self.image_name)))
        infile = tmpdir.join("input.st")
        f = open(str(infile), 'w')
        f.write("Utilities setAuthorInitials: 'foo'. SmallInteger compile: 'jittestNow\r\n%s'.\r\nSmalltalk snapshot: true andQuit: true." % code.replace("'", "''"))
        f.close()
        curdir = os.getcwd()
        os.chdir(str(tmpdir))
        try:
            squeak.system(self.image_name, infile)
        finally:
            os.chdir(curdir)

        logfile = str(tmpdir.join("x.pypylog"))
        print logfile
        proc = spy.popen(
            "--reader-jit-args", "off", "-n", "0", "-m", "jittestNow", self.image_name,
            cwd=str(tmpdir),
            env={"PYPYLOG": "jit-log-opt,jit-summary:%s" % logfile,
                 "SDL_VIDEODRIVER": "dummy"}
        )
        proc.wait()
        return logparser.extract_traces(logfile)


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
