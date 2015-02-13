from rpython.rlib import jit

# In addition to marking the decorated function as "pure", both the receiver
# and the version of the receiver are promoted to constants. This should only
# be used in situations where the receiver is very unlikely to change in the same
# context of the interpreted program (like classes or compiled methods).
def constant_for_version(func):
    def versioned_func(self, version):
        return func(self)
    versioned_func.func_name = "constant_" + func.func_name
    elidable_func = jit.elidable_promote()(versioned_func)
    def meth(self):
        return elidable_func(self, self.version)
    meth.func_name = "constant_meth_" + func.func_name
    return meth

# Same as constant_for_version, but allows for one additional argument.
def constant_for_version_arg(func):
    def versioned_func(self, version, arg):
        return func(self, arg)
    versioned_func.func_name = "constant_" + func.func_name
    elidable_func = jit.elidable_promote()(versioned_func)
    def meth(self, arg):
        return elidable_func(self, self.version, arg)
    meth.func_name = "constant_meth_" + func.func_name
    return meth

class Version(object):
    pass

class VersionMixin(object):
    # Concrete class must define a pseudo immutable field like the following:
    # _attrs_ = ['version']
    # _immutable_fields_ = ['version?']
    
    version = Version()
    
    def changed(self):
        self.version = Version()
