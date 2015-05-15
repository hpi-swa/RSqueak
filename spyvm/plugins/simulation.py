from spyvm import model_display, model
from spyvm.error import PrimitiveFailedError, MetaPrimFailed
from spyvm.storage import AbstractCachingShadow
from spyvm.plugins.plugin import Plugin

from rpython.rlib import jit, objectmodel
from rpython.rlib.rarithmetic import r_uint, intmask

# If an EXTERNAL_CALL for the given moduleName and functionName is not found,
# the SimulationPlugin is used to enable the image simulating that primitive.
# For that, the simulatePrimitive: aFunctionName args: aCollection message is sent
# to the receiver of the EXTERNAL_CALL primitive.
# Note that the moduleName parameter of EXTERNAL_CALL is omitted and has no influence on the simulation!

# simulatePrimitive:args: may return a value to pass it to the primitive caller and
# should raise primitive 255 META_PRIM_FAILED (e.g. via InterpreterProxy>>primitiveFailFor:)
# if the primitive failed and the fallback code should be run.

# Example:
# BitBlt>>simulatePrimitive: aFunctionName args: aCollection
#     aFunctionName = 'primitiveCopyBits' ifTrue: [ ^self copyBitsSimulated ].
#     aFunctionName = 'primitiveWarpBits' ifTrue: [ ^self warpBitsSimulated: (args at: 1) sourceMap: (args at: 2) ].
#     ^InterpreterProxy new primitiveFailFor: 255.

class SimulationPluginClass(Plugin):
    def simulate(self, w_name, name, interp, s_frame, argcount, w_method):
        if not interp.image.w_simulatePrimitive:
            raise PrimitiveFailedError("No Simulation found")

        from spyvm.interpreter import Return
        from spyvm.error import MethodNotFound

        w_arguments = s_frame.peek_n(argcount)
        w_rcvr = s_frame.peek(argcount)

        s_class = w_rcvr.class_shadow(interp.space)
        try:
            s_class.lookup(interp.image.w_simulatePrimitive)
        except MethodNotFound:
            raise PrimitiveFailedError("%s doesn't implement %s" % (s_class.getname(), "simulatePrimitive:args:"))

        s_frame.push(w_rcvr)
        s_frame.push(w_name)
        s_frame.push(interp.space.wrap_list_unroll_safe(w_arguments))

        s_fallback = w_method.create_frame(interp.space, w_rcvr, w_arguments)
        s_fallback._s_sender = s_frame

        try:
            s_frame._sendSelector(interp.image.w_simulatePrimitive, 2, interp, w_rcvr, w_rcvr.class_shadow(interp.space), s_fallback=s_fallback)
        except Return, ret:
            # must clean the stack, including the rcvr
            s_frame.pop_n(argcount + 1)
            s_frame.push(ret.value)
            return w_rcvr
        except MetaPrimFailed, e:
            interp.stack_frame(s_fallback, None)
            return w_rcvr
        else:
            raise PrimitiveFailedError


SimulationPlugin = SimulationPluginClass()
