"""Immutable W_WordsObject Implementation."""

from rsqueakvm.model.base import W_AbstractObjectWithClassReference
from rsqueakvm.model.variable import W_WordsObject
from rsqueakvm.plugins.immutability import immutable_class


@immutable_class
class W_Immutable_WordsObject(W_WordsObject):
    """`W_WordsObject` subclass with immutable words."""
    _immutable_fields_ = ['immutable_words']
    repr_classname = '%s_Immutable' % W_WordsObject.repr_classname

    def __init__(self, space, w_cls, words):
        """
        Initialize immutable words object and store its words in
        `self.immutable_words` slot.
        `W_Immutable_WordsObject.__init__(self, space, w_class, size)` not
        called, because there is no need to initialize `self.words`.
        """
        W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
        self.immutable_words = words

    # No need to make this jit.elidable, jit can prove return val is constant.
    def _words(self):
        """
        `W_WordsObject._words(self)` override.

        :returns: words from `self.immutable_words` slot.
        """
        return self.immutable_words

    """
    No need to override other methods that reference self.words, because they
    were stubbed out by @immutable_class.
    """
