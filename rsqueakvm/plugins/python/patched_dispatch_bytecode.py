from rpython.rlib import jit
from rpython.rlib.rarithmetic import r_uint, intmask

from pypy.interpreter.pycode import BytecodeCorruption
from pypy.interpreter.pyopcode import SReturnValue, Return, SuspendedUnroller
from pypy.tool.stdlib_opcode import bytecode_spec

opcodedesc = bytecode_spec.opcodedesc
HAVE_ARGUMENT = bytecode_spec.HAVE_ARGUMENT


@jit.unroll_safe
def dispatch_bytecode(self, co_code, next_instr, ec):
    while True:
        # PythonPlugin patch start
        val = self.smalltalk_check()
        if val >= 0:
            return val
        # PythonPlugin patch end

        self.last_instr = intmask(next_instr)
        if jit.we_are_jitted():
            ec.bytecode_only_trace(self)
        else:
            ec.bytecode_trace(self)
        next_instr = r_uint(self.last_instr)
        opcode = ord(co_code[next_instr])
        next_instr += 1

        if opcode >= HAVE_ARGUMENT:
            lo = ord(co_code[next_instr])
            hi = ord(co_code[next_instr+1])
            next_instr += 2
            oparg = (hi * 256) | lo
        else:
            oparg = 0

        # note: the structure of the code here is such that it makes
        # (after translation) a big "if/elif" chain, which is then
        # turned into a switch().

        while opcode == opcodedesc.EXTENDED_ARG.index:
            opcode = ord(co_code[next_instr])
            if opcode < HAVE_ARGUMENT:
                raise BytecodeCorruption
            lo = ord(co_code[next_instr+1])
            hi = ord(co_code[next_instr+2])
            next_instr += 3
            oparg = (oparg * 65536) | (hi * 256) | lo

        if opcode == opcodedesc.RETURN_VALUE.index:
            w_returnvalue = self.popvalue()
            block = self.unrollstack(SReturnValue.kind)
            if block is None:
                self.pushvalue(w_returnvalue)   # XXX ping pong
                raise Return
            else:
                unroller = SReturnValue(w_returnvalue)
                next_instr = block.handle(self, unroller)
                return next_instr    # now inside a 'finally' block
        elif opcode == opcodedesc.END_FINALLY.index:
            unroller = self.end_finally()
            if isinstance(unroller, SuspendedUnroller):
                # go on unrolling the stack
                block = self.unrollstack(unroller.kind)
                if block is None:
                    w_result = unroller.nomoreblocks()
                    self.pushvalue(w_result)
                    raise Return
                else:
                    next_instr = block.handle(self, unroller)
            return next_instr
        elif opcode == opcodedesc.JUMP_ABSOLUTE.index:
            return self.jump_absolute(oparg, ec)
        elif opcode == opcodedesc.BREAK_LOOP.index:
            next_instr = self.BREAK_LOOP(oparg, next_instr)
        elif opcode == opcodedesc.CONTINUE_LOOP.index:
            return self.CONTINUE_LOOP(oparg, next_instr)
        elif opcode == opcodedesc.FOR_ITER.index:
            next_instr = self.FOR_ITER(oparg, next_instr)
        elif opcode == opcodedesc.JUMP_FORWARD.index:
            next_instr = self.JUMP_FORWARD(oparg, next_instr)
        elif opcode == opcodedesc.JUMP_IF_FALSE_OR_POP.index:
            next_instr = self.JUMP_IF_FALSE_OR_POP(oparg, next_instr)
        elif opcode == opcodedesc.JUMP_IF_NOT_DEBUG.index:
            next_instr = self.JUMP_IF_NOT_DEBUG(oparg, next_instr)
        elif opcode == opcodedesc.JUMP_IF_TRUE_OR_POP.index:
            next_instr = self.JUMP_IF_TRUE_OR_POP(oparg, next_instr)
        elif opcode == opcodedesc.POP_JUMP_IF_FALSE.index:
            next_instr = self.POP_JUMP_IF_FALSE(oparg, next_instr)
        elif opcode == opcodedesc.POP_JUMP_IF_TRUE.index:
            next_instr = self.POP_JUMP_IF_TRUE(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_ADD.index:
            self.BINARY_ADD(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_AND.index:
            self.BINARY_AND(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_DIVIDE.index:
            self.BINARY_DIVIDE(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_FLOOR_DIVIDE.index:
            self.BINARY_FLOOR_DIVIDE(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_LSHIFT.index:
            self.BINARY_LSHIFT(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_MODULO.index:
            self.BINARY_MODULO(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_MULTIPLY.index:
            self.BINARY_MULTIPLY(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_OR.index:
            self.BINARY_OR(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_POWER.index:
            self.BINARY_POWER(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_RSHIFT.index:
            self.BINARY_RSHIFT(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_SUBSCR.index:
            self.BINARY_SUBSCR(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_SUBTRACT.index:
            self.BINARY_SUBTRACT(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_TRUE_DIVIDE.index:
            self.BINARY_TRUE_DIVIDE(oparg, next_instr)
        elif opcode == opcodedesc.BINARY_XOR.index:
            self.BINARY_XOR(oparg, next_instr)
        elif opcode == opcodedesc.BUILD_CLASS.index:
            self.BUILD_CLASS(oparg, next_instr)
        elif opcode == opcodedesc.BUILD_LIST.index:
            self.BUILD_LIST(oparg, next_instr)
        elif opcode == opcodedesc.BUILD_LIST_FROM_ARG.index:
            self.BUILD_LIST_FROM_ARG(oparg, next_instr)
        elif opcode == opcodedesc.BUILD_MAP.index:
            self.BUILD_MAP(oparg, next_instr)
        elif opcode == opcodedesc.BUILD_SET.index:
            self.BUILD_SET(oparg, next_instr)
        elif opcode == opcodedesc.BUILD_SLICE.index:
            self.BUILD_SLICE(oparg, next_instr)
        elif opcode == opcodedesc.BUILD_TUPLE.index:
            self.BUILD_TUPLE(oparg, next_instr)
        elif opcode == opcodedesc.CALL_FUNCTION.index:
            self.CALL_FUNCTION(oparg, next_instr)
        elif opcode == opcodedesc.CALL_FUNCTION_KW.index:
            self.CALL_FUNCTION_KW(oparg, next_instr)
        elif opcode == opcodedesc.CALL_FUNCTION_VAR.index:
            self.CALL_FUNCTION_VAR(oparg, next_instr)
        elif opcode == opcodedesc.CALL_FUNCTION_VAR_KW.index:
            self.CALL_FUNCTION_VAR_KW(oparg, next_instr)
        elif opcode == opcodedesc.CALL_METHOD.index:
            self.CALL_METHOD(oparg, next_instr)
        elif opcode == opcodedesc.COMPARE_OP.index:
            self.COMPARE_OP(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_ATTR.index:
            self.DELETE_ATTR(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_FAST.index:
            self.DELETE_FAST(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_GLOBAL.index:
            self.DELETE_GLOBAL(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_NAME.index:
            self.DELETE_NAME(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_SLICE_0.index:
            self.DELETE_SLICE_0(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_SLICE_1.index:
            self.DELETE_SLICE_1(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_SLICE_2.index:
            self.DELETE_SLICE_2(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_SLICE_3.index:
            self.DELETE_SLICE_3(oparg, next_instr)
        elif opcode == opcodedesc.DELETE_SUBSCR.index:
            self.DELETE_SUBSCR(oparg, next_instr)
        elif opcode == opcodedesc.DUP_TOP.index:
            self.DUP_TOP(oparg, next_instr)
        elif opcode == opcodedesc.DUP_TOPX.index:
            self.DUP_TOPX(oparg, next_instr)
        elif opcode == opcodedesc.EXEC_STMT.index:
            self.EXEC_STMT(oparg, next_instr)
        elif opcode == opcodedesc.GET_ITER.index:
            self.GET_ITER(oparg, next_instr)
        elif opcode == opcodedesc.IMPORT_FROM.index:
            self.IMPORT_FROM(oparg, next_instr)
        elif opcode == opcodedesc.IMPORT_NAME.index:
            self.IMPORT_NAME(oparg, next_instr)
        elif opcode == opcodedesc.IMPORT_STAR.index:
            self.IMPORT_STAR(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_ADD.index:
            self.INPLACE_ADD(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_AND.index:
            self.INPLACE_AND(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_DIVIDE.index:
            self.INPLACE_DIVIDE(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_FLOOR_DIVIDE.index:
            self.INPLACE_FLOOR_DIVIDE(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_LSHIFT.index:
            self.INPLACE_LSHIFT(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_MODULO.index:
            self.INPLACE_MODULO(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_MULTIPLY.index:
            self.INPLACE_MULTIPLY(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_OR.index:
            self.INPLACE_OR(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_POWER.index:
            self.INPLACE_POWER(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_RSHIFT.index:
            self.INPLACE_RSHIFT(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_SUBTRACT.index:
            self.INPLACE_SUBTRACT(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_TRUE_DIVIDE.index:
            self.INPLACE_TRUE_DIVIDE(oparg, next_instr)
        elif opcode == opcodedesc.INPLACE_XOR.index:
            self.INPLACE_XOR(oparg, next_instr)
        elif opcode == opcodedesc.LIST_APPEND.index:
            self.LIST_APPEND(oparg, next_instr)
        elif opcode == opcodedesc.LOAD_ATTR.index:
            self.LOAD_ATTR(oparg, next_instr)
        elif opcode == opcodedesc.LOAD_CLOSURE.index:
            self.LOAD_CLOSURE(oparg, next_instr)
        elif opcode == opcodedesc.LOAD_CONST.index:
            self.LOAD_CONST(oparg, next_instr)
        elif opcode == opcodedesc.LOAD_DEREF.index:
            self.LOAD_DEREF(oparg, next_instr)
        elif opcode == opcodedesc.LOAD_FAST.index:
            self.LOAD_FAST(oparg, next_instr)
        elif opcode == opcodedesc.LOAD_GLOBAL.index:
            self.LOAD_GLOBAL(oparg, next_instr)
        elif opcode == opcodedesc.LOAD_LOCALS.index:
            self.LOAD_LOCALS(oparg, next_instr)
        elif opcode == opcodedesc.LOAD_NAME.index:
            self.LOAD_NAME(oparg, next_instr)
        elif opcode == opcodedesc.LOOKUP_METHOD.index:
            self.LOOKUP_METHOD(oparg, next_instr)
        elif opcode == opcodedesc.MAKE_CLOSURE.index:
            self.MAKE_CLOSURE(oparg, next_instr)
        elif opcode == opcodedesc.MAKE_FUNCTION.index:
            self.MAKE_FUNCTION(oparg, next_instr)
        elif opcode == opcodedesc.MAP_ADD.index:
            self.MAP_ADD(oparg, next_instr)
        elif opcode == opcodedesc.NOP.index:
            self.NOP(oparg, next_instr)
        elif opcode == opcodedesc.POP_BLOCK.index:
            self.POP_BLOCK(oparg, next_instr)
        elif opcode == opcodedesc.POP_TOP.index:
            self.POP_TOP(oparg, next_instr)
        elif opcode == opcodedesc.PRINT_EXPR.index:
            self.PRINT_EXPR(oparg, next_instr)
        elif opcode == opcodedesc.PRINT_ITEM.index:
            self.PRINT_ITEM(oparg, next_instr)
        elif opcode == opcodedesc.PRINT_ITEM_TO.index:
            self.PRINT_ITEM_TO(oparg, next_instr)
        elif opcode == opcodedesc.PRINT_NEWLINE.index:
            self.PRINT_NEWLINE(oparg, next_instr)
        elif opcode == opcodedesc.PRINT_NEWLINE_TO.index:
            self.PRINT_NEWLINE_TO(oparg, next_instr)
        elif opcode == opcodedesc.RAISE_VARARGS.index:
            self.RAISE_VARARGS(oparg, next_instr)
        elif opcode == opcodedesc.ROT_FOUR.index:
            self.ROT_FOUR(oparg, next_instr)
        elif opcode == opcodedesc.ROT_THREE.index:
            self.ROT_THREE(oparg, next_instr)
        elif opcode == opcodedesc.ROT_TWO.index:
            self.ROT_TWO(oparg, next_instr)
        elif opcode == opcodedesc.SETUP_EXCEPT.index:
            self.SETUP_EXCEPT(oparg, next_instr)
        elif opcode == opcodedesc.SETUP_FINALLY.index:
            self.SETUP_FINALLY(oparg, next_instr)
        elif opcode == opcodedesc.SETUP_LOOP.index:
            self.SETUP_LOOP(oparg, next_instr)
        elif opcode == opcodedesc.SETUP_WITH.index:
            self.SETUP_WITH(oparg, next_instr)
        elif opcode == opcodedesc.SET_ADD.index:
            self.SET_ADD(oparg, next_instr)
        elif opcode == opcodedesc.SLICE_0.index:
            self.SLICE_0(oparg, next_instr)
        elif opcode == opcodedesc.SLICE_1.index:
            self.SLICE_1(oparg, next_instr)
        elif opcode == opcodedesc.SLICE_2.index:
            self.SLICE_2(oparg, next_instr)
        elif opcode == opcodedesc.SLICE_3.index:
            self.SLICE_3(oparg, next_instr)
        elif opcode == opcodedesc.STOP_CODE.index:
            self.STOP_CODE(oparg, next_instr)
        elif opcode == opcodedesc.STORE_ATTR.index:
            self.STORE_ATTR(oparg, next_instr)
        elif opcode == opcodedesc.STORE_DEREF.index:
            self.STORE_DEREF(oparg, next_instr)
        elif opcode == opcodedesc.STORE_FAST.index:
            self.STORE_FAST(oparg, next_instr)
        elif opcode == opcodedesc.STORE_GLOBAL.index:
            self.STORE_GLOBAL(oparg, next_instr)
        elif opcode == opcodedesc.STORE_MAP.index:
            self.STORE_MAP(oparg, next_instr)
        elif opcode == opcodedesc.STORE_NAME.index:
            self.STORE_NAME(oparg, next_instr)
        elif opcode == opcodedesc.STORE_SLICE_0.index:
            self.STORE_SLICE_0(oparg, next_instr)
        elif opcode == opcodedesc.STORE_SLICE_1.index:
            self.STORE_SLICE_1(oparg, next_instr)
        elif opcode == opcodedesc.STORE_SLICE_2.index:
            self.STORE_SLICE_2(oparg, next_instr)
        elif opcode == opcodedesc.STORE_SLICE_3.index:
            self.STORE_SLICE_3(oparg, next_instr)
        elif opcode == opcodedesc.STORE_SUBSCR.index:
            self.STORE_SUBSCR(oparg, next_instr)
        elif opcode == opcodedesc.UNARY_CONVERT.index:
            self.UNARY_CONVERT(oparg, next_instr)
        elif opcode == opcodedesc.UNARY_INVERT.index:
            self.UNARY_INVERT(oparg, next_instr)
        elif opcode == opcodedesc.UNARY_NEGATIVE.index:
            self.UNARY_NEGATIVE(oparg, next_instr)
        elif opcode == opcodedesc.UNARY_NOT.index:
            self.UNARY_NOT(oparg, next_instr)
        elif opcode == opcodedesc.UNARY_POSITIVE.index:
            self.UNARY_POSITIVE(oparg, next_instr)
        elif opcode == opcodedesc.UNPACK_SEQUENCE.index:
            self.UNPACK_SEQUENCE(oparg, next_instr)
        elif opcode == opcodedesc.WITH_CLEANUP.index:
            self.WITH_CLEANUP(oparg, next_instr)
        elif opcode == opcodedesc.YIELD_VALUE.index:
            self.YIELD_VALUE(oparg, next_instr)
        else:
            self.MISSING_OPCODE(oparg, next_instr)

        if jit.we_are_jitted():
            return next_instr
