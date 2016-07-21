import readline
from rpython.rlib import objectmodel, unroll



COMMANDS = []
HELP = []


def cmd(func):
    COMMANDS.append(func.__name__)
    HELP.append(func.__doc__)
    return func


def untranslated_cmd(func):
    if objectmodel.we_are_translated():
        def m(s, c): return
        return m
    else:
        return cmd(func)


class Shell(object):
    def __init__(self, interp, space):
        self.interp = interp
        self.space = space
        self.methods = {}
        space.headless.activate()

    @cmd
    def exit(self, code):
        "!exit for quitting"
        exit(0)

    @untranslated_cmd
    def pdb(self, code):
        "!pdb to drop to python shell"
        import pdb; pdb.set_trace()

    @cmd
    def help(self, code):
        "!help to print this help"
        for h in HELP:
            print h

    @untranslated_cmd
    def reload(self, code):
        "!reload(...) to reload a python file"
        pass

    @cmd
    def method(self, code):
        "!method Class to define a method. End with !!"
        parts = code.split(" ", 1)
        if len(parts) == 2:
            w_cls = self._execute_code(parts[1])
            methodsrc = []
            srcline = ""
            while srcline != "!!":
                srcline = raw_input("%s| " % parts[1]).strip()
                methodsrc.append(srcline)
            from targetrsqueak import compile_code
            methodsrc.pop() # remove trailing !!
            compile_code(self.interp, w_cls,
                         "\r\n".join(methodsrc),
                         isclass=True, make_selector=False)
        else:
            print "Wrong syntax, type !help for help"

    def run(self):
        print "You're in a Smalltalk REPL. Type `!exit' to quit, !help for help."
        while True:
            code = raw_input("$ ").strip()
            if code.startswith("!"):
                method = code[1:].split(" ")[0]
                for n in unroll.unrolling_iterable(COMMANDS):
                    if n == method:
                        getattr(self, n)(code)
                        break
            else:
                w_result = self._execute_code(code)
                if w_result:
                    print w_result.as_repr_string().replace('\r', '\n')

    def _execute_code(self, code):
        from targetrsqueak import compile_code, execute_context
        w_selector = self.methods.get(code, None)
        if not w_selector:
            w_selector = self.interp.perform(
                self.space.wrap_string(compile_code(self.interp, self.space.w_nil, "^ %s" % code)),
                "asSymbol")
            self.methods[code] = w_selector
        s_frame = self.interp.create_toplevel_context(
            self.space.w_nil, w_selector=w_selector, w_arguments=[])
        return execute_context(self.interp, s_frame)
