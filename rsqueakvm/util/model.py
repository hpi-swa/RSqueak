def has_immutable_subclass(cls):
    class ImmutableSubclass(cls):
        _attrs_ = ['_frozen']
        _frozen = False

        def freeze(self):
            self._frozen = True

        def is_immutable(self):
            return self._frozen

    ImmutableSubclass.__name__ = '%s_Immutable' % cls.__name__

    cls.immutable_class = ImmutableSubclass
    return cls


def no_immutable_access(fn):
    def decorator(self, *args):
        if not self.is_immutable():
            fn(self, *args)
    return decorator
