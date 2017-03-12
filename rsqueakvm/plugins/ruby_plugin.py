from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.ruby.plugin import RubyPlugin

from rpython.rlib import jit

try:
    from rsqueakvm.plugins.ruby import ruby_space, utils
    from rsqueakvm.plugins.ruby.model import W_RubyObject

    from topaz.error import RubyError, print_traceback
except ImportError:
    pass


plugin = RubyPlugin()


# @plugin.expose_primitive(unwrap_spec=[object])
# def getTopFrame(interp, s_frame, w_rcvr):
#     # import pdb; pdb.set_trace()
#     topframe = ruby_space.getexecutioncontext().gettoprubyframe()
#     if topframe is None:
#         raise PrimitiveFailedError
#     TODO: topframe is not a topaz.objects.objectobject.W_Root object
#     return W_RubyObject(topframe)


@plugin.expose_primitive(compiled_method=True)
@jit.unroll_safe
def send(interp, s_frame, argcount, w_method):
    # import pdb; pdb.set_trace()
    space = interp.space
    args_w = s_frame.peek_n(argcount)
    args_rw = [utils.smalltalk_to_ruby(space, w_arg) for w_arg in args_w]
    wr_rcvr = utils.smalltalk_to_ruby(space, s_frame.peek(argcount))
    w_selector_name = w_method.literalat0(space, 2)
    if not isinstance(w_selector_name, W_BytesObject):
        print 'w_selector_name not an instance of W_BytesObject'
        raise PrimitiveFailedError
    methodname = space.unwrap_string(w_selector_name)
    idx = methodname.find(':')
    if idx > 0:
        methodname = methodname[0:idx]

    try:
        wr_result = ruby_space.send(wr_rcvr, methodname, args_w=args_rw)
        return W_RubyObject(wr_result)
    except RubyError as e:
        print_traceback(ruby_space, e.w_value)
        raise PrimitiveFailedError
