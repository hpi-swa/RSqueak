from spyvm import model_display, model
from spyvm.error import PrimitiveFailedError, MetaPrimFailed, MethodNotFound
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

from spyvm.constants import SIMULATE_PRIMITIVE_SELECTOR

class SimulationPluginClass(Plugin):
    def _simulate(self, w_name, interp, s_frame, argcount, w_method):
        w_arguments = s_frame.peek_n(argcount)
        w_rcvr = s_frame.peek(argcount)

        s_class = w_rcvr.class_shadow(interp.space)

        if not interp.image.w_simulatePrimitive or interp.image.w_simulatePrimitive.is_nil(interp.space):
            raise PrimitiveFailedError("Primitive %s has failed and no %s>>%s was found (Selector not in image)" % (w_name, s_class.getname(), SIMULATE_PRIMITIVE_SELECTOR))

        try:
            s_class.lookup(interp.image.w_simulatePrimitive)
        except MethodNotFound:
            raise PrimitiveFailedError("Primitive %s has failed and no %s>>%s was found" % (w_name, s_class.getname(), SIMULATE_PRIMITIVE_SELECTOR))

        s_frame.push(w_rcvr)
        s_frame.push(w_name)
        s_frame.push(interp.space.wrap_list_unroll_safe(w_arguments))

        s_fallback = w_method.create_frame(interp.space, w_rcvr, w_arguments)
        s_fallback._s_sender = s_frame

        from spyvm.interpreter import Return

        try:
            s_frame._sendSelector(interp.image.w_simulatePrimitive, 2, interp, w_rcvr, w_rcvr.class_shadow(interp.space), s_fallback=s_fallback)
        except Return, ret:
            # must clean the stack, including the rcvr
            s_frame.pop_n(argcount + 1)
            s_frame.push(ret.value)
            return w_rcvr

    def simulate(self, w_name, signature, interp, s_frame, argcount, w_method):
        self._simulate(w_name, interp, s_frame, argcount, w_method)

    def simulateNumeric(self, code, interp, s_frame, argcount, w_method):
        self._simulate(interp.space.wrap_int(code), interp, s_frame, argcount, w_method)

SimulationPlugin = SimulationPluginClass()
