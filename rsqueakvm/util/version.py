from rpython.rlib import jit

# In addition to marking the decorated function as "pure", by default both the
# receiver, the version, and all arguments are promoted to constants. This
# should only be used in situations where the receiver is very unlikely to
# change in the same context of the interpreted program (like classes or
# compiled methods).
def elidable_for_version(numargs, promote='all'):
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
        if promote is not None:
            elidable_func = jit.elidable_promote(promote)(versioned_func)
        else:
            elidable_func = jit.elidable(versioned_func)
        code = [
            "def meth(self %s):" % argstr
        ]
        if promote and (promote == 'all' or ("0" in promote.split(","))):
            code.append("    self = jit.promote(self)")
        code.append(
            "    return elidable_func(self, self.version %s)" % argstr
        )
        d = {"elidable_func": elidable_func, "jit": jit}
        exec "\n".join(code) in d
        meth = d["meth"]
        meth.func_name = "constant_meth_" + func.func_name
        return meth
    return decorator

# This decorator calls the elidable version of the function only if cond is
# true. This is useful, for example, to first check that self is a jit constant,
# and otherwise trace into the original function.
def elidable_for_version_iff(numargs, promote='all', cond=None):
    def decorator(func):
        elidable_func = elidable_for_version(numargs, promote=promote)(func)
        argstr = "".join([", arg%d" % i for i in range(numargs)])
        code = [
            "def cond_versioned_func(self %s):" % argstr,
            "    if cond(self %s):" % argstr,
            "        return elidable_func(self %s)" % argstr,
            "    else:",
            "        return func(self %s)" % argstr
        ]
        d = {"func": func, "elidable_func": elidable_func, "cond": cond}
        exec "\n".join(code) in d
        cond_versioned_func = d["cond_versioned_func"]
        cond_versioned_func.func_name = "cond_constant_method_" + func.func_name
        return cond_versioned_func
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
