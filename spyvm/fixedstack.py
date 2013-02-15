"""
A Fixed stack for SPy.
"""

import types

from rpython.rlib import jit
from rpython.rlib.rarithmetic import r_uint

class FixedStack(object):
    # _annspecialcase_ = "specialize:ctr_location" # polymorphic
    
    def __init__(self):
        pass
    
    def setup(self, stacksize):
        self.ptr = r_uint(0) # we point after the last element
        self.items = [None] * stacksize
    
    def clone(self):
        # this is only needed if we support flow space
        s = self.__class__()
        s.setup(len(self.items))
        for item in self.items[:self.ptr]:
            try:
                item = item.clone()
            except AttributeError:
                pass
            s.push(item)
        return s
    
    def push(self, item):
        ptr = jit.promote(self.ptr)
        self.items[ptr] = item
        self.ptr = ptr + 1
    
    def pop(self):
        ptr = jit.promote(self.ptr) - 1
        ret = self.items[ptr]   # you get OverflowError if the stack is empty
        self.items[ptr] = None
        self.ptr = ptr
        return ret

    @jit.unroll_safe
    def drop(self, n):
        jit.promote(self.ptr)
        while n > 0:
            n -= 1
            self.ptr -= 1
            self.items[self.ptr] = None
    
    def top(self, position=0):
        # for a fixed stack, we assume correct indices
        rpos = r_uint(position)
        return self.items[self.ptr + ~rpos]
    
    def set_top(self, value, position=0):
        # for a fixed stack, we assume correct indices
        rpos = r_uint(position)
        self.items[self.ptr + ~rpos] = value
    
    def depth(self):
        return self.ptr
    
    def empty(self):
        return not self.ptr
    

