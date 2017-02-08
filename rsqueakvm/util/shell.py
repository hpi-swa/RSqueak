try:
    import readline
except ImportError:
    pass # Win32
import re
import sys
import os
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
            "block_closure": None,
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
            "mirror": None,
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
    msg = "The !%s command is not available after translation." % func.__name__
    cmdfunc = cmd(func)
    def m(s, c):
        if objectmodel.we_are_translated():
            print msg
            return
        else:
            return cmdfunc(s, c)
    return m


class Shell(object):
    def __init__(self, interp, space, code=None):
        self.set_interp(interp)
        self.space = space
        self.methods = {}
        self.w_rcvr = self.space.w_nil
        self.last_result = None
        if not code:
            self.current_code = []
        else:
            self.current_code = [code]
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
        from rpython.rlib.nonconst import NonConstant
        os._exit(NonConstant(0))

    @untranslated_cmd
    def pdb(self, code):
        "!pdb to drop to python shell"
        import pdb; pdb.set_trace()

    @cmd
    def help(self, code):
        "!help to print this help"
        print "Empty lines and lines that start and end with '\"' are skipped."
        print "Lines that do not start with ! are run as Smalltalk."
        print
        print "In addition, the following commands are available:"
        for h in HELP:
            print h

    @cmd
    def trace(self, code):
        "!trace on|off to enable/disable interp tracing"
        if "on" in code:
            self.interp.trace = True
        elif "off" in code:
            self.interp.trace = False
        else:
            print "Error in command syntax"
        return

    @cmd
    def load(self, code):
        "!load Filename to read and execute a file"
        from rpython.rlib.streamio import open_file_as_stream
        code = code.split(" ", 1)
        if len(code) != 2:
            print "Error in command syntax"
            return
        path = code[1]
        try:
            f = open_file_as_stream(path, buffering=0)
        except OSError as e:
            os.write(2, "%s -- %s (LoadError)\n" % (os.strerror(e.errno), path))
            return
        try:
            source = f.readall()
        finally:
            f.close()
        self.current_code = source.split("\n")

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
                    codeobj = compile(
                        "\n".join([
                            re.sub("^" + "\\s" * outdent, "", line) \
                            for line in inspect.getsource(method).split("\n")
                        ]),
                        inspect.getsourcefile(method),
                        'exec'
                    )
                    exec(codeobj, modmod.__dict__, l)
                    try:
                        setattr(oldclasses[classname], methodname, l[methodname])
                    except KeyError:
                        print "Error updating " + classname + "#" + methodname + ". Maybe try again?"
                        pass
        elif code.startswith("rsqueakvm.interpreter"):
            import rsqueakvm.interpreter
            import rsqueakvm.interpreter_bytecodes
            reload(rsqueakvm.interpreter_bytecodes)
            reload(rsqueakvm.interpreter)
            self.set_interp(rsqueakvm.interpreter.Interpreter(
                self.space, self.interp.image,
                self.interp.trace, self.interp.trace_important,
                self.interp.evented, self.interp.interrupts))
        else:
            print "Cannot reload %s" % code
            return
        print "Reloaded %s" % code

    def raw_input(self, delim):
        if len(self.current_code) > 0:
            new_line = self.current_code.pop(0)
            print new_line
            return new_line
        self.set_readline()
        try:
            if not objectmodel.we_are_translated():
                return raw_input(delim)

            os.write(1, delim)
            line = []
            c = os.read(0, 1)
            while c != "\n":
                line.append(c)
                c = os.read(0, 1)
            return "".join(line)
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
                methodsrc.append(srcline)
            from rsqueakvm.main import compile_code
            methodsrc.pop() # remove trailing !!
            compile_code(self.interp, w_cls,
                         "\r\n".join(methodsrc),
                         isclass=True, make_selector=False)
        else:
            print "Wrong syntax, type !help for help"

    def run(self):
        print "You're in a Smalltalk REPL. Type `!exit' to quit, !help for help."
        while True:
            code = self.raw_input("$ ").strip()
            if code.startswith("!"):
                method = code[1:].split(" ")[0]
                for n in UNROLLING_COMMANDS:
                    if n == method:
                        getattr(self, n)(code)
            elif len(code) == 0:
                pass
            elif code.startswith('"') and code.endswith('"'):
                pass
            else:
                if not objectmodel.we_are_translated():
                    import traceback
                w_result = None
                try:
                    w_result = self._execute_code(code)
                except:
                    if not objectmodel.we_are_translated():
                        print traceback.format_exc()
                        import pdb; pdb.set_trace()
                    else:
                        print "Error"
                if w_result:
                    self.last_result = w_result
                    print w_result.as_repr_string().replace('\r', '\n')

    def _execute_code(self, code):
        from rsqueakvm.main import compile_code, execute_context
        w_selector = self.methods.get(code, None)
        if not w_selector:
            os.write(1, "Compiling code... ")
            w_selector = self.interp.perform(
                self.space.wrap_string(compile_code(self.interp, self.w_rcvr, "^ %s" % code)),
                "asSymbol")
            self.methods[code] = w_selector
            print "...done."
        s_frame = self.interp.create_toplevel_context(
            self.w_rcvr, w_selector=w_selector, w_arguments=[])
        return execute_context(self.interp, s_frame)


UNROLLING_COMMANDS = unroll.unrolling_iterable(COMMANDS)
