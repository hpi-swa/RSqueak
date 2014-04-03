from rpython.rlib import jit

# This declares the decorated function as "pure" while the self-object
# has an unchanged version. Neither self nor self.version are promoted to constants.
def elidable_for_version(func):
    @jit.elidable
    def elidable_func(self, version, *args):
        return func(self, *args)
    def meth(self, *args):
        return elidable_func(self, self.version, *args)
    elidable_func.func_name = "elidable_" + func.func_name
    meth.func_name = "elidable_meth_" + func.func_name
    return meth

# In addition to marking the decorated function as "pure", both the receiver
# and the version of the receiver are promoted to constants. This should only
# be used in situations where the receiver is very unlikely to change in the same
# context of the interpreted program (like classes or compiled methods).
def constant_for_version(func):
    @jit.elidable
    def elidable_func(self, version, *args):
        return func(self, *args)
    def meth(self, *args):
        self = jit.promote(self)
        version = jit.promote(self.version)
        return elidable_func(self, version, *args)
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
