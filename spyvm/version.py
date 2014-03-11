from rpython.rlib import jit

def elidable_after_versioning(func):
    @jit.elidable
    def elidable_func(self, version, *args):
        return func(self, *args)
    def meth(self, *args):
        jit.promote(self)
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
