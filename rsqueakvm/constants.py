import sys
import time
from rpython.rlib.rarithmetic import r_int64, r_uint

from rsqueakvm.util.bitmanipulation import splitter

# ___________________________________________________________________________
# Slot Names

CHARACTER_VALUE_INDEX = 0        # Page 630 of the blue book

STREAM_ARRAY_INDEX = 0           # Page 631 of the blue book
STREAM_INDEX_INDEX = 1
STREAM_READ_LIMIT_INDEX = 2
STREAM_WRITE_LIMIT_INDEX = 3

CLASS_SUPERCLASS_INDEX = 0
CLASS_METHODDICT_INDEX = 1
CLASS_FORMAT_INDEX = 2
CLASS_NAME_INDEX = 6                # in the mini.image, at least

# MethodDict
METHODDICT_TALLY_INDEX = 0
METHODDICT_VALUES_INDEX = 1
METHODDICT_NAMES_INDEX = 2

# Message
MESSAGE_SELECTOR_INDEX = 0
MESSAGE_ARGUMENTS_INDEX = 1
MESSAGE_LOOKUP_CLASS_INDEX = 2

# ContextPart
CTXPART_SENDER_INDEX = 0
CTXPART_PC_INDEX = 1
CTXPART_STACKP_INDEX = 2

METHOD_HEADER_INDEX = 0

# BlockContext < ContextPart
BLKCTX_BLOCK_ARGUMENT_COUNT_INDEX = 3
BLKCTX_INITIAL_IP_INDEX = 4
BLKCTX_HOME_INDEX = 5
BLKCTX_STACK_START = 6

# MethodContext < ContextPart
MTHDCTX_METHOD = 3
MTHDCTX_CLOSURE_OR_NIL = 4
MTHDCTX_RECEIVER = 5
MTHDCTX_TEMP_FRAME_START = 6

# BlockClosure < Object
BLKCLSR_OUTER_CONTEXT = 0
BLKCLSR_STARTPC = 1
BLKCLSR_NUMARGS = 2
BLKCLSR_SIZE = 3

FORM_BITS = 0
FORM_WIDTH = 1
FORM_HEIGHT = 2
FORM_DEPTH = 3

# ___________________________________________________________________________
# Miscellaneous constants

COMPILED_METHOD_FULL_FRAME_SIZE = 56
COMPILED_METHOD_SMALL_FRAME_SIZE = 16
LITERAL_START = 1  # index of the first literal after the method header
BYTES_PER_WORD = 4
WORDS_IN_FLOAT = 2  # Fixed number of word-slots in a Squeak Float object
INTERP_PROXY_MAJOR = 1
INTERP_PROXY_MINOR = 13

# The Delta between Squeak Epoch (Jan 1st 1901) and POSIX Epoch (Jan 1st 1970)
SQUEAK_EPOCH_DELTA_MICROSECONDS = r_int64(2177452800000000L)

# ___________________________________________________________________________
# Special objects indices

SPECIAL_OBJECTS_SIZE = 70 # some room

SO_NIL = 0
SO_FALSE = 1
SO_TRUE = 2
SO_SCHEDULERASSOCIATIONPOINTER = 3
SO_BITMAP_CLASS = 4
SO_SMALLINTEGER_CLASS = 5
SO_STRING_CLASS = 6
SO_ARRAY_CLASS = 7
SO_SMALLTALK = 8  # Deprecated
SO_FLOAT_CLASS = 9
SO_METHODCONTEXT_CLASS = 10
SO_BLOCKCONTEXT_CLASS = 11
SO_POINT_CLASS = 12
SO_LARGEPOSITIVEINTEGER_CLASS = 13
SO_DISPLAY_OBJECT = 14
SO_MESSAGE_CLASS = 15
SO_COMPILEDMETHOD_CLASS = 16
SO_LOW_SPACE_SEMAPHORE = 17
SO_SEMAPHORE_CLASS = 18
SO_CHARACTER_CLASS = 19
SO_DOES_NOT_UNDERSTAND = 20
SO_CANNOT_RETURN = 21
SO_PROCESS_SIGNALIGN_LOW_SPACE = 22  # the process that triggered the low space semaphore. mostly nil
SO_SPECIAL_SELECTORS_ARRAY = 23
SO_CHARACTER_TABLE_ARRAY = 24
SO_MUST_BE_BOOLEAN = 25
SO_BYTEARRAY_CLASS = 26
SO_PROCESS_CLASS = 27
SO_COMPACT_CLASSES_ARRAY = 28
SO_TIMER_SEMAPHORE = 29
SO_USER_INTERRUPT_SEMAPHORE = 30
SO_FLOAT_ZERO = 31
SO_LARGEPOSITIVEINTEGER_ZERO = 32
SO_A_POINT = 33
SO_CANNOT_INTERPRET = 34
SO_A_METHODCONTEXT = 35  # deprecated in closure images
SO_BLOCKCLOSURE_CLASS = 36
SO_A_BLOCKCONTEXT = 37  # deprecated in closure images
SO_EXTERNAL_OBJECTS_ARRAY = 38
SO_PSEUDOCONTEXT_CLASS = 39
SO_TRANSLATEDMETHOD_CLASS = 40
SO_FINALIZATION_SEMPAHORE = 41
SO_LARGENEGATIVEINTEGER_CLASS = 42
SO_RUN_WITH_IN = 49
SO_JIT_HOOK = 58 # really selectorCounterTripped
SO_JIT_HOOK_RCVR = 59 # really selectorTrap

constant_objects_in_special_object_table = {
    "nil": (SO_NIL, "POINTERS"),
    "true": (SO_TRUE, "POINTERS"),
    "false": (SO_FALSE, "POINTERS"),
    "charactertable": (SO_CHARACTER_TABLE_ARRAY, "POINTERS"),
    "schedulerassociationpointer": (SO_SCHEDULERASSOCIATIONPOINTER, "POINTERS"),
    "special_selectors": (SO_SPECIAL_SELECTORS_ARRAY, "POINTERS"),
    "smalltalkdict": (SO_SMALLTALK, "POINTERS"),
    "doesNotUnderstand": (SO_DOES_NOT_UNDERSTAND, "BYTES"),
    "mustBeBoolean": (SO_MUST_BE_BOOLEAN, "BYTES"),
    "runWithIn": (SO_RUN_WITH_IN, "BYTES"),
    "cannotReturn": (SO_CANNOT_RETURN, "BYTES"),
    # classes
    "Bitmap": (SO_BITMAP_CLASS, "POINTERS"),
    "SmallInteger": (SO_SMALLINTEGER_CLASS, "POINTERS"),
    "String": (SO_STRING_CLASS, "POINTERS"),
    "Array": (SO_ARRAY_CLASS, "POINTERS"),
    "Float": (SO_FLOAT_CLASS, "POINTERS"),
    "MethodContext": (SO_METHODCONTEXT_CLASS, "POINTERS"),
    "BlockContext": (SO_BLOCKCONTEXT_CLASS, "POINTERS"),
    "BlockClosure": (SO_BLOCKCLOSURE_CLASS, "POINTERS"),
    "Point": (SO_POINT_CLASS, "POINTERS"),
    "LargePositiveInteger": (SO_LARGEPOSITIVEINTEGER_CLASS, "POINTERS"),
    "Message": (SO_MESSAGE_CLASS, "POINTERS"),
    "CompiledMethod": (SO_COMPILEDMETHOD_CLASS, "POINTERS"),
    "Semaphore": (SO_SEMAPHORE_CLASS, "POINTERS"),
    "Character": (SO_CHARACTER_CLASS, "POINTERS"),
    "ByteArray": (SO_BYTEARRAY_CLASS, "POINTERS"),
    "Process": (SO_PROCESS_CLASS, "POINTERS"),
#    "PseudoContext" : (SO_PSEUDOCONTEXT_CLASS, "POINTERS"),
#    "TranslatedMethod" : (SO_TRANSLATEDMETHOD_CLASS, "POINTERS"),
    "LargeNegativeInteger" : (SO_LARGENEGATIVEINTEGER_CLASS, "POINTERS"),
    # ours, not in the table, but we'd like it to
    "ClassBinding": (SPECIAL_OBJECTS_SIZE + 30, "POINTERS"),
    "Metaclass": (SPECIAL_OBJECTS_SIZE + 31, "POINTERS"),
    "Processor": (SPECIAL_OBJECTS_SIZE + 32, "POINTERS"),
}

variables_in_special_object_table = {
    "display": SO_DISPLAY_OBJECT,
    "interrupt_semaphore": SO_USER_INTERRUPT_SEMAPHORE,
    "timerSemaphore": SO_TIMER_SEMAPHORE,
    "jit_hook_selector": SO_JIT_HOOK,
    "jit_hook_receiver": SO_JIT_HOOK_RCVR
}

def init_special_objects_mapping(constant_objects_in_special_object_table):
    d = {}
    for name, (idx, t) in constant_objects_in_special_object_table.items():
        d[name] = idx
    return d
constant_objects_in_special_object_table_wo_types = init_special_objects_mapping(constant_objects_in_special_object_table)
objects_in_special_object_table = {}
objects_in_special_object_table.update(constant_objects_in_special_object_table_wo_types)
objects_in_special_object_table.update(variables_in_special_object_table)


from rpython.rlib.rarithmetic import LONG_BIT
TAGGED_MAXINT = 2 ** (LONG_BIT - 2) - 1
TAGGED_MININT = -2 ** (LONG_BIT - 2)

TAGGED_MAXINT32 = 2 ** (32 - 2) - 1
TAGGED_MININT32 = -2 ** (32 - 2)

TAGGED_MASK = int(2 ** (LONG_BIT - 1) - 1)

MAXINT = sys.maxint
MININT = -sys.maxint-1
U_MAXINT = r_uint(2 ** LONG_BIT - 1)

if LONG_BIT == 32:
    IS_64BIT = False
    BYTES_PER_MACHINE_INT = 4
    BYTES_PER_MACHINE_LONGLONG = 8
elif LONG_BIT == 64:
    IS_64BIT = True
    BYTES_PER_MACHINE_INT = 8
    BYTES_PER_MACHINE_LONGLONG = 8
else:
    assert False

# Entries into SO_SPECIAL_SELECTORS_ARRAY:
#(#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1 #* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1 #at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0 #blockCopy: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0)


SPECIAL_SELECTORS = ['+', '-', '<', '>', '<=', '>=', '=', '~=', '*', '/', '\\\\',
                     '@', 'bitShift:', '//', 'bitAnd:', 'bitOr:', 'at:',
                     'at:put:', 'size', 'next', 'nextPut:', 'atEnd', '==',
                     'class', 'blockCopy:', 'value', 'value:', 'do:', 'new',
                     'new:', 'x', 'y']

def find_selectorindex(selector):
    return SPECIAL_SELECTORS.index(selector) * 2
find_selectorindex._annspecialcase_ = "specialize:memo"

def decode_compiled_method_header(header):
    """Decode 30-bit method header and apply new format.

    (index 0)  9 bits: main part of primitive number   (#primitive)
    (index 9)  8 bits: number of literals (#numLiterals)
    (index 17) 1 bit:  whether a large frame size is needed (#frameSize)
    (index 18) 6 bits: number of temporary variables (#numTemps)
    (index 24) 4 bits: number of arguments to the method (#numArgs)
    (index 28) 1 bit:  high-bit of primitive number (#primitive)
    (index 29) 1 bit:  flag bit, ignored by the VM  (#flag)
    """
    if header < 0: # sign bit == 1 (bit index depends on 32b/64b)
        return decode_alternate_compiled_method_header(header)
    primitive, literalsize, islarge, tempsize, numargs, highbit = (
        splitter[9,8,1,6,4,1](header))
    primitive = primitive + (highbit << 9)
    assert tempsize >= numargs
    return primitive, literalsize, islarge, tempsize, numargs

def decode_alternate_compiled_method_header(header):
    """Decode 30-bit method header and apply new format.

    (index 0)   16 bits:        number of literals (#numLiterals)
    (index 16)    1 bit:        has primitive
    (index 17)    1 bit:        whether a large frame size is needed (#frameSize)
    (index 18)    6 bits:       number of temporary variables (#numTemps)
    (index 24)    4 bits:       number of arguments to the method (#numArgs)
    (index 28)    2 bits:       reserved for an access modifier (00-unused, 01-private, 10-protected, 11-public)
    """
    literalsize, has_primitive, islarge, tempsize, numargs, access_mod = (
            splitter[16,1,1,6,4,2](header))
    return bool(has_primitive), literalsize, islarge, tempsize, numargs

#___________________________________________________________________________
# Interpreter constants
#

INTERRUPT_COUNTER_SIZE = 10000
CompileTime = time.time()

SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX = 1
SYSTEM_ATTRIBUTE_IMAGE_ARGS_INDEX = 2
