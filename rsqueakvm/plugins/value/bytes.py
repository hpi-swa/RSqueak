# """Immutable W_BytesObject Implementation."""

# from rsqueakvm.model.base import W_AbstractObjectWithClassReference
# from rsqueakvm.model.variable import W_BytesObject
# from rsqueakvm.plugins.immutability import immutable_class


# @immutable_class
# class W_Immutable_BytesObject(W_BytesObject):
#     """`W_BytesObject` subclass with immutable bytes."""
#     _immutable_fields_ = ['immutable_bytes']
#     repr_classname = '%s_Immutable' % W_BytesObject.repr_classname

#     def __init__(self, space, w_cls, bytes):
#         """
#         Initialize immutable bytes object and store its bytes in
#         `self.immutable_bytes` slot.
#         `W_BytesObject.__init__(self, space, w_class, size)` not called,
#         because there is no need to `self.mutate()` and set `self.bytes`.
#         """
#         W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
#         self.immutable_bytes = bytes

#     # No need to make this jit.elidable, jit can prove return val is constant.
#     def _bytes(self):
#         """
#         `W_BytesObject._bytes(self)` override.

#         :returns: bytes from `self.immutable_bytes` slot.
#         """
#         return self.immutable_bytes

#     def _version(self):
#         """
#         `W_BytesObject._version(self)` override.

#         :returns: `None`.
#         """
#         return None

#     """
#     No need to override other methods that reference self.bytes, because they
#     were stubbed out by @immutable_class.
#     """
