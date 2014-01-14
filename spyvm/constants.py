from rpython.rlib.jit import elidable

from spyvm.tool.bitmanipulation import splitter

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
METHODDICT_NAMES_INDEX  = 2

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

# ___________________________________________________________________________
# Miscellaneous constants

LITERAL_START = 1 # index of the first literal after the method header
BYTES_PER_WORD = 4

# ___________________________________________________________________________
# Special objects indices

SO_NIL = 0
SO_FALSE = 1
SO_TRUE = 2
SO_SCHEDULERASSOCIATIONPOINTER = 3
SO_BITMAP_CLASS = 4
SO_SMALLINTEGER_CLASS = 5
SO_STRING_CLASS = 6
SO_ARRAY_CLASS = 7
SO_SMALLTALK = 8 # Deprecated
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
SO_PROCESS_SIGNALIGN_LOW_SPACE = 22 # the process that triggered the low space semaphore. mostly nil
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
SO_A_METHODCONTEXT = 35 # deprecated in closure images
SO_BLOCKCLOSURE_CLASS = 36
SO_A_BLOCKCONTEXT = 37 # deprecated in closure images
SO_EXTERNAL_OBJECTS_ARRAY = 38
SO_PSEUDOCONTEXT_CLASS = 39
SO_TRANSLATEDMETHOD_CLASS = 40
SO_FINALIZATION_SEMPAHORE = 41
SO_LARGENEGATIVEINTEGER_CLASS = 42

# XXX more missing?
classes_in_special_object_table = {
    "Bitmap" : SO_BITMAP_CLASS,
    "SmallInteger" : SO_SMALLINTEGER_CLASS,
    "String" : SO_STRING_CLASS,
    "Array" : SO_ARRAY_CLASS,
    "Float" : SO_FLOAT_CLASS,
    "MethodContext" : SO_METHODCONTEXT_CLASS,
    "BlockContext" : SO_BLOCKCONTEXT_CLASS,
    "BlockClosure" : SO_BLOCKCLOSURE_CLASS,
    "Point" : SO_POINT_CLASS,
    "LargePositiveInteger" : SO_LARGEPOSITIVEINTEGER_CLASS,
    "Message" : SO_MESSAGE_CLASS,
    "CompiledMethod" : SO_COMPILEDMETHOD_CLASS,
    "Semaphore" : SO_SEMAPHORE_CLASS,
    "Character" : SO_CHARACTER_CLASS,
    "ByteArray" : SO_BYTEARRAY_CLASS,
    "Process" : SO_PROCESS_CLASS,
#    "PseudoContext" : SO_PSEUDOCONTEXT_CLASS,
#    "TranslatedMethod" : SO_TRANSLATEDMETHOD_CLASS,
    # "LargeNegativeInteger" : SO_LARGENEGATIVEINTEGER_CLASS, # Not available in mini.image
}

objects_in_special_object_table = {
    "nil": SO_NIL,
    "true": SO_TRUE,
    "false": SO_FALSE,
    "charactertable": SO_CHARACTER_TABLE_ARRAY,
    "schedulerassociationpointer" : SO_SCHEDULERASSOCIATIONPOINTER,
    "special_selectors": SO_SPECIAL_SELECTORS_ARRAY,
    "smalltalkdict" : SO_SMALLTALK,
    "display" : SO_DISPLAY_OBJECT,
    "doesNotUnderstand" : SO_DOES_NOT_UNDERSTAND,
    "interrupt_semaphore" : SO_USER_INTERRUPT_SEMAPHORE,
    "timerSemaphore" : SO_TIMER_SEMAPHORE,
}

LONG_BIT = 32
TAGGED_MAXINT = 2 ** (LONG_BIT - 2) - 1
TAGGED_MININT = -2 ** (LONG_BIT - 2)

TAGGED_MASK = int(2 ** (LONG_BIT - 1) - 1)


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
    primitive, literalsize, islarge, tempsize, numargs, highbit = (
        splitter[9,8,1,6,4,1](header))
    primitive = primitive + (highbit << 10) ##XXX todo, check this
    assert tempsize >= numargs
    return primitive, literalsize, islarge, tempsize, numargs

#___________________________________________________________________________
# Interpreter constants
#

MAX_LOOP_DEPTH = 100
INTERRUPT_COUNTER_SIZE = 10000
