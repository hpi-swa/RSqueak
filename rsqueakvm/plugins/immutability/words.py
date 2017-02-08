from rsqueakvm.model.base import W_AbstractObjectWithClassReference
from rsqueakvm.model.variable import W_WordsObject
from rsqueakvm.plugins.immutability.utils import immutable_class

from rpython.rlib import jit


@immutable_class
class W_Immutable_WordsObject(W_WordsObject):
    repr_classname = '%s_Immutable' % W_WordsObject.repr_classname

    def __init__(self, space, w_cls, words_w):
        W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
        self.words = words_w

    @jit.elidable
    def _words(self):
        return W_WordsObject._words(self)
