from rsqueakvm.primitives import expose_alternative_primitive, prim_table
from rsqueakvm.primitives.constants import *

def expose_mirror_primitive(code):
    original = prim_table[code]
    def func(interp, s_frame, argument_count, w_method=None):
        # ignore the receiver
        original(interp, s_frame, argument_count - 1, w_method=w_method)
        # get the result
        w_result = s_frame.pop()
        s_frame.pop() # remove rcvr
        return w_result
    expose_alternative_primitive(code, clean_stack=False)(func)

# These are used by the debugger. They all have an extra first argument, the
# object on which to execute
for code in [AT, AT_PUT, EQUIVALENT, INST_VAR_AT, INST_VAR_AT_PUT, CLASS, SIZE]:
    expose_mirror_primitive(code)
