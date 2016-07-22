import readline
import re
import sys
import inspect
from rpython.rlib import objectmodel, unroll
from rsqueakvm.error import Exit


COMMANDS = []
HELP = []
autocompletions = {
    "rsqueakvm": {
        "interpreter": None,
        "model": {
            "base": None,
            "character": None,
            "compiled_methods": None,
            "display": None,
            "numeric": None,
            "pointers": None,
            "variable": None,
        },
        "plugins": None,
        "primitives": {
            "arithmetic": None,
            "array_stream": None,
            "block_closure": None,
            "bytecodes": None,
            "control": None,
            "input_output": None,
            "misc": None,
            "storage": None,
            "system": None,
        }
    }
}


def cmd(func):
    COMMANDS.append(func.__name__)
    HELP.append(func.__doc__)
    autocompletions["!%s" % func.__name__] = None
    return func


def completer(text, state, completions=None):
    "NOT RPYTHON"
    if not completions:
        completions = autocompletions
    matches = 0
    for k,v in completions.items():
        pk = "%s." % k
        if k.find(text) == 0:
            if matches == state:
                return pk if v else k
            else:
                matches += 1
        elif text.find(pk) == 0 and v:
            subtext = text.replace(pk, "", 1)
            subkey = completer(subtext, state, completions=v)
            return "%s%s" % (pk, subkey) if subkey else None
    return None


def untranslated_cmd(func):
    if objectmodel.we_are_translated():
        def m(s, c): return
        return m
    else:
        return cmd(func)


class Shell(object):
    def __init__(self, interp, space):
        self.set_interp(interp)
        self.space = space
        self.methods = {}
        self.w_rcvr = self.space.w_nil
        space.headless.activate()

    def set_interp(self, interp):
        if not objectmodel.we_are_translated():
            interp.shell_execute = True
        self.interp = interp

    def set_readline(self):
        if not objectmodel.we_are_translated():
            self.old_completer = readline.get_completer()
            self.old_delims = readline.get_completer_delims()
            readline.set_completer(completer)
            readline.set_completer_delims("\t ")

    def reset_readline(self):
        if not objectmodel.we_are_translated():
            readline.set_completer(self.old_completer)
            readline.set_completer_delims(self.old_delims)

    @cmd
    def q(self, code):
        "!q for quitting"
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
        "!reload rsqueakvm.abc.xyz... to reload some VM code"
        code = code.split(" ", 1)
        if len(code) != 2:
            print "Error in command syntax"
            return
        code = code[1]
        if code.startswith("rsqueakvm.plugins"):
            import rsqueakvm.primitives.control
            reload(rsqueakvm.primitives.control)
        elif code.startswith("rsqueakvm.primitives."):
            primmod = __import__(code, fromlist=["rsqueakvm.primitives"])
            reload(primmod)
        elif code.startswith("rsqueakvm.model."):
            modmod = __import__(code, fromlist=["rsqueakvm.model"])
            # This does not do a reload, and thus gives the old classes
            oldclasses = inspect.getmembers(
                modmod, lambda x: inspect.isclass(x) and inspect.getmodule(x) is modmod)
            reload(modmod)
            newclasses = inspect.getmembers(
                modmod, lambda x: inspect.isclass(x) and inspect.getmodule(x) is modmod)
            for k,v in oldclasses:
                setattr(modmod, k, v) # patch old classes into module again
            oldclasses = dict(oldclasses)
            for classname,klass in newclasses:
                methods = inspect.getmembers(klass, lambda x: inspect.ismethod(x))
                for methodname,method in methods:
                    if methodname.startswith("__"):
                        continue
                    # define new method in module
                    l = {}
                    outdent = re.match("^\\s*", inspect.getsource(method)).end()
                    code = compile(
                        "\n".join([
                            re.sub("^" + "\\s" * outdent, "", line) \
                            for line in inspect.getsource(method).split("\n")
                        ]),
                        inspect.getsourcefile(method),
                        'exec'
                    )
                    exec(code, modmod.__dict__, l)
                    try:
                        setattr(oldclasses[classname], methodname, l[methodname])
                    except KeyError:
                        print "Error updating " + methodname + ". Maybe try again?"
                        pass
        elif code.startswith("rsqueakvm.interpreter"):
            import rsqueakvm.interpreter
            import rsqueakvm.interpreter_bytecodes
            reload(rsqueakvm.interpreter_bytecodes)
            reload(rsqueakvm.interpreter)
            self.set_interp(interpreter.Interpreter(
                self.space, self.interp.image,
                self.interp.trace, self.interp.trace_important,
                self.interp.evented, self.interp.interrupts))
        else:
            print "Cannot reload %s" % code
            return
        print "Reloaded %s" % code

    def raw_input(self, delim):
        self.set_readline()
        try:
            return raw_input(delim).strip()
        finally:
            self.reset_readline()

    @cmd
    def method(self, code):
        "!method Class to define a method. End with !!"
        parts = code.split(" ", 1)
        if len(parts) == 2:
            w_cls = self._execute_code(parts[1])
            methodsrc = []
            srcline = ""
            while srcline != "!!":
                srcline = self.raw_input("%s| " % parts[1])
                if srcline: # don't record method source as history
                    readline.remove_history_item(readline.get_current_history_length() - 1)
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
            code = self.raw_input("$ ")
            if code.startswith("!"):
                method = code[1:].split(" ")[0]
                for n in unroll.unrolling_iterable(COMMANDS):
                    if n == method:
                        getattr(self, n)(code)
            else:
                try:
                    w_result = self._execute_code(code)
                except:
                    print "Error: ", sys.exc_info()[0]
                    import pdb; pdb.set_trace()
                if w_result:
                    print w_result.as_repr_string().replace('\r', '\n')

    def _execute_code(self, code):
        from targetrsqueak import compile_code, execute_context
        w_selector = self.methods.get(code, None)
        if not w_selector:
            sys.stdout.write("Compiling code... ")
            sys.stdout.flush()
            w_selector = self.interp.perform(
                self.space.wrap_string(compile_code(self.interp, self.w_rcvr, "^ %s" % code)),
                "asSymbol")
            self.methods[code] = w_selector
            print "...done."
            sys.stdout.flush()
        s_frame = self.interp.create_toplevel_context(
            self.w_rcvr, w_selector=w_selector, w_arguments=[])
        return execute_context(self.interp, s_frame)
