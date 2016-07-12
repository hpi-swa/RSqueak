from rsqueakvm.error import SimulatedPrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.model.compiled_methods import W_CompiledMethod

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

SIMULATE_PRIMITIVE_SELECTOR = 'simulatePrimitive:args:'

class SimulationPluginClass(Plugin):
    def _simulate(self, w_name, interp, s_frame, argcount, w_method):
        w_rcvr = s_frame.peek(argcount)
        s_class = w_rcvr.class_shadow(interp.space)
        if not interp.image.w_simulatePrimitive or interp.image.w_simulatePrimitive.is_nil(interp.space):
            raise SimulatedPrimitiveFailedError("Primitive has failed and simulator selector not in image", w_name, s_class)
        w_sim_method = s_class.lookup(interp.image.w_simulatePrimitive)
        if w_sim_method is None:
            raise SimulatedPrimitiveFailedError("Primitive has failed and no simulator method was found on this class", w_name, s_class)
        if not isinstance(w_sim_method, W_CompiledMethod):
            raise SimulatedPrimitiveFailedError("Simulator method must be an ordinary compiled method", w_name, s_class)

        w_arguments = s_frame.pop_and_return_n(argcount)
        s_frame.pop() # remove receiver

        s_fallback = w_method.create_frame(interp.space, w_rcvr, w_arguments)
        s_fallback._s_sender = s_frame
        s_sim_frame = w_sim_method.create_frame(
            interp.space,
            w_rcvr,
            [w_name, interp.space.wrap_list_unroll_safe(w_arguments)],
            s_fallback=s_fallback)
        interp.stack_frame(s_sim_frame, s_frame)

    def simulate(self, w_name, signature, interp, s_frame, argcount, w_method):
        self._simulate(w_name, interp, s_frame, argcount, w_method)

    def simulateNumeric(self, code, interp, s_frame, argcount, w_method):
        self._simulate(interp.space.wrap_int(code), interp, s_frame, argcount, w_method)

SimulationPlugin = SimulationPluginClass()
