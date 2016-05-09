from rpython.rlib import jit

# In addition to marking the decorated function as "pure", by default both the
# receiver, the version, and all arguments are promoted to constants. This
# should only be used in situations where the receiver is very unlikely to
# change in the same context of the interpreted program (like classes or
# compiled methods).
def elidable_for_version(numargs, promote=True):
    def decorator(func):
        argstr = "".join([", arg%d" % i for i in range(numargs)])
        code = [
            "def versioned_func(self, version %s):" % argstr,
            "    return func(self %s)" % argstr
        ]
        d = {"func": func}
        exec "\n".join(code) in d
        versioned_func = d["versioned_func"]
        versioned_func.func_name = "constant_" + func.func_name
        if promote:
            elidable_func = jit.elidable_promote()(versioned_func)
        else:
            elidable_func = jit.elidable(versioned_func)
        code = [
            "def meth(self %s):" % argstr,
            "    return elidable_func(self, self.version %s)" % argstr
        ]
        d = {"elidable_func": elidable_func}
        exec "\n".join(code) in d
        meth = d["meth"]
        meth.func_name = "constant_meth_" + func.func_name
        return meth
    return decorator

class Version(object):
    pass

class VersionMixin(object):
    # Concrete class must define a pseudo immutable field like the following:
    # _attrs_ = ['version']
    # _immutable_fields_ = ['version?']

    version = Version()

    def changed(self):
        self.version = Version()
