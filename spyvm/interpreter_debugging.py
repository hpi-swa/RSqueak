
import pdb
from spyvm.storage_contexts import ContextPartShadow
from spyvm import model, constants, primitives

# This module patches up the interpreter and adds breakpoints at certain execution points.
# Only usable in interpreted mode due to pdb.
# To use, execute one of following after interpreter.py is loaded:
# from spyvm import interpreter_debugging; interpreter_debugging.activate_debugging()
# or, before Interpreter instance is created:
# Interpreter.__init__ = interpreter_debugging.activating_init(Interpreter.__init__)

# After this, following flags control whether the interpreter breaks at the respective locations:
# <interp> can be an interpreter instance or the Interpreter class
# interp.step_bytecodes
# interp.step_sends
# interp.step_returns
# interp.step_primitives
# interp.step_failed_primitives
# interp.step_failed_named_primitives

def activating_init(original):
    def meth(*args):
        activate_debugging()
        return original(*args)
    return meth

def activate_debugging():
    from spyvm.interpreter import Interpreter
    Interpreter.step_bytecodes = False
    Interpreter.step_sends = False
    Interpreter.step_returns = False
    Interpreter.step_primitives = False
    Interpreter.step_failed_primitives = False
    
    _break = pdb.set_trace
    
    def patch(obj):
        def do_patch(meth):
            name = meth.__name__
            original = getattr(obj, name)
            assert original, "Object %r does not have a method named %s" % (obj, name)
            replacement = meth(original)
            setattr(obj, name, replacement)
            return meth
        return do_patch
    
    patch_context = patch(ContextPartShadow)
    
    @patch_context
    def debug_bytecode(original):
        def meth(self, interp):
            if interp.step_bytecodes:
                _break() # Continue stepping from here to get to the current bytecode execution
        return meth
    
    @patch_context
    def _sendSelector(original):
        def meth(self, w_selector, argcount, interp, receiver, receiverclassshadow, w_arguments=None):
            if interp.step_sends:
                _break() # Continue stepping from here to get to the current message send
            return original(self, w_selector, argcount, interp, receiver, receiverclassshadow, w_arguments=w_arguments)
        return meth
    
    @patch_context
    def _return(original):
        def meth(self, return_value, interp, local_return=False):
            if interp.step_returns:
                _break() # Continue stepping from here to get to the current return
            return original(self, return_value, interp, local_return=local_return)
        return meth
    
    @patch_context
    def _call_primitive(original):
        def meth(self, code, interp, argcount, w_method, w_selector):
            if interp.step_primitives:
                _break() # Continue stepping from here to get to the current primitive
            try:
                return original(self, code, interp, argcount, w_method, w_selector)
            except error.PrimitiveFailedError, e:
                if interp.step_failed_primitives:
                    _break() # Continue stepping from here to get to the current failed primitive.
                    
                    # Should fail again.
                    original(self, code, interp, argcount, w_method, w_selector)
        return meth
    
    def failed_named_primitive(original):
        def meth(interp, s_frame, argcount, w_method=None):
            try:
                return original(interp, s_frame, argcount, w_method=w_method)
            except error.PrimitiveFailedError, e:
                if interp.step_failed_named_primitives:
                    _break() # Continue from here to get to the current failed named primitive.
                    
                    space = interp.space
                    w_description = w_method.literalat0(space, 1)
                    if isinstance(w_description, model.W_PointersObject) and w_description.size() >= 2:
                        w_modulename = w_description.at0(space, 0)
                        w_functionname = w_description.at0(space, 1)
                        print "Failed named primitive. Module: %s, Function: %s" % (w_modulename, w_functionname)
                    
                    # Should fail again.
                    original(interp, s_frame, argcount, w_method=w_method)
                raise e
        return meth
    
    primitives.prim_table[primitives.EXTERNAL_CALL] = failed_named_primitive(primitives.prim_table[primitives.EXTERNAL_CALL])
    