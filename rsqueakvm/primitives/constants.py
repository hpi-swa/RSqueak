# ___________________________________________________________________________
# SmallInteger Primitives
ADD = 1
SUBTRACT = 2
LESSTHAN = 3
GREATERTHAN = 4
LESSOREQUAL = 5
GREATEROREQUAL = 6
EQUAL = 7
NOTEQUAL = 8
MULTIPLY = 9
DIVIDE = 10
MOD = 11
DIV = 12
QUO = 13
BIT_AND = 14
BIT_OR = 15
BIT_XOR = 16
BIT_SHIFT = 17
MAKE_POINT = 18
FAIL = 19
LARGE_OFFSET = 20
LARGE_REM = 20
LARGE_ADD = 21
LARGE_SUBTRACT = 22
LARGE_LESSTHAN = 23
LARGE_GREATERTHAN = 24
LARGE_LESSOREQUAL = 25
LARGE_GREATEROREQUAL = 26
LARGE_EQUAL = 27
LARGE_NOTEQUAL = 28
LARGE_MULTIPLY = 29
LARGE_DIVIDE = 30
LARGE_MOD = 31
LARGE_DIV = 32
LARGE_QUO = 33
LARGE_BIT_AND = 34
LARGE_BIT_OR = 35
LARGE_BIT_XOR = 36
LARGE_BIT_SHIFT = 37
# ___________________________________________________________________________
# Float Primitives
FLOAT_OFFSET = 40
SMALLINT_AS_FLOAT = 40
FLOAT_ADD = 41
FLOAT_SUBTRACT = 42
FLOAT_LESSTHAN = 43
FLOAT_GREATERTHAN = 44
FLOAT_LESSOREQUAL = 45
FLOAT_GREATEROREQUAL = 46
FLOAT_EQUAL = 47
FLOAT_NOTEQUAL = 48
FLOAT_MULTIPLY = 49
FLOAT_DIVIDE = 50
FLOAT_TRUNCATED = 51
# OPTIONAL: 52, 53
FLOAT_TIMES_TWO_POWER = 54
FLOAT_SQUARE_ROOT = 55
FLOAT_SIN = 56
FLOAT_ARCTAN = 57
FLOAT_LOG_N = 58
FLOAT_EXP = 59
# ___________________________________________________________________________
# Subscript and Stream Primitives
AT = 60
AT_PUT = 61
SIZE = 62
STRING_AT = 63
STRING_AT_PUT = 64
# ___________________________________________________________________________
# Stream Primitives
NEXT = 65
NEXT_PUT = 66
AT_END = 67
# ___________________________________________________________________________
# Storage Management Primitives
OBJECT_AT = 68
OBJECT_AT_PUT = 69
NEW = 70
NEW_WITH_ARG = 71
ARRAY_BECOME_ONE_WAY = 72     # Blue Book: primitiveBecome
INST_VAR_AT = 73
INST_VAR_AT_PUT = 74
AS_OOP = 75
STORE_STACKP = 76             # Blue Book: primitiveAsObject
SOME_INSTANCE = 77
NEXT_INSTANCE = 78
NEW_METHOD = 79
# ___________________________________________________________________________
# Control Primitives
BLOCK_COPY = 80
VALUE = 81
VALUE_WITH_ARGS = 82
PERFORM = 83
PERFORM_WITH_ARGS = 84
PERFORM_IN_SUPERCLASS = 100
SIGNAL = 85
WAIT = 86
RESUME = 87
SUSPEND = 88
FLUSH_CACHE = 89
# ___________________________________________________________________________
# I/O Primitives
MOUSE_POINT = 90
TEST_DISPLAY_DEPTH = 91
SET_DISPLAY_MODE = 92
INPUT_SEMAPHORE = 93
GET_NEXT_EVENT = 94
INPUT_WORD = 95
BITBLT_COPY_BITS = 96
SNAPSHOT = 97
STORE_IMAGE_SEGMENT = 98
LOAD_IMAGE_SEGMENT = 99
BE_CURSOR = 101
BE_DISPLAY = 102
SCAN_CHARACTERS = 103
OBSOLETE_INDEXED = 104  # also 96
REPLACE_FROM_TO = 105
SCREEN_SIZE = 106
MOUSE_BUTTONS = 107
KBD_NEXT = 108
KBD_PEEK = 109
# ___________________________________________________________________________
# Control Primitives
EQUIVALENT = 110
CLASS = 111
BYTES_LEFT = 112
QUIT = 113
EXIT_TO_DEBUGGER = 114
CHANGE_CLASS = 115      # Blue Book: primitiveOopsLeft
COMPILED_METHOD_FLUSH_CACHE = 116
EXTERNAL_CALL = 117
SYMBOL_FLUSH_CACHE = 119
# ___________________________________________________________________________
# Miscellaneous Primitives
CALLOUT_TO_FFI = 120
IMAGE_NAME = 121
NOOP = 122
VALUE_UNINTERRUPTABLY = 123
LOW_SPACE_SEMAPHORE = 124
SIGNAL_AT_BYTES_LEFT = 125
DEFER_UPDATES = 126
DRAW_RECTANGLE = 127
# ___________________________________________________________________________
# Squeak Miscellaneous Primitives
BECOME = 128
SPECIAL_OBJECTS_ARRAY = 129
FULL_GC = 130
INC_GC = 131
SET_INTERRUPT_KEY = 133
INTERRUPT_SEMAPHORE = 134
# ____________________________________________________________________________
# Time Primitives
MILLISECOND_CLOCK = 135
SIGNAL_AT_MILLISECONDS = 136
SECONDS_CLOCK = 137
# ____________________________________________________________________________
# Misc Primitives
SOME_OBJECT = 138
NEXT_OBJECT = 139
BEEP = 140
CLIPBOARD_TEXT = 141
VM_PATH = 142
SHORT_AT = 143
SHORT_AT_PUT = 144
FILL = 145
CLONE = 148
SYSTEM_ATTRIBUTE = 149
# ___________________________________________________________________________
# File primitives
# (XXX they are obsolete in Squeak and done with a plugin)
FILE_AT_END = 150
FILE_CLOSE = 151
FILE_GET_POSITION = 152
FILE_OPEN = 153
FILE_READ = 154
FILE_SET_POSITION = 155
FILE_DELETE = 156
FILE_SIZE = 157
FILE_WRITE = 158
FILE_RENAME = 159
DIRECTORY_CREATE = 160
DIRECTORY_DELIMITOR = 161
DIRECTORY_LOOKUP = 162
DIRECTORY_DELTE = 163
# ___________________________________________________________________________
# Misc primitives
YIELD = 167
INTEGER_AT = 165
INTEGER_AT_PUT = 166
CHARACTER_VALUE = 170
IMMEDIATE_IDENTITY_HASH = 171
SLOT_AT = 173
SLOT_AT_PUT = 174
CLASS_IDENTITY_HASH = 175
MAX_IDENTITY_HASH = 176
ALL_INSTANCES = 177
ALL_OBJECTS = 178
BYTE_SIZE_OF_INSTANCE = 181
EXIT_CRITICAL_SECTION = 185  # similar to SIGNAL, hence SIGNAL + 100
ENTER_CRITICAL_SECTION = 186  # similar to WAIT, hence WAIT + 100
TEST_AND_SET_OWNERSHIP_OF_CRITICAL_SECTION = 187
WITH_ARGS_EXECUTE_METHOD = 188
# ___________________________________________________________________________
# BlockClosure Primitives
CLOSURE_COPY_WITH_COPIED_VALUES = 200
CLOSURE_VALUE = 201
CLOSURE_VALUE_ = 202
CLOSURE_VALUE_VALUE = 203
CLOSURE_VALUE_VALUE_VALUE = 204
CLOSURE_VALUE_VALUE_VALUE_VALUE = 205
CLOSURE_VALUE_WITH_ARGS = 206  # valueWithArguments:
# ___________________________________________________________________________
# Override the default primitive to give latitude to the VM in context management.
CTXT_AT = 210
CTXT_AT_PUT = 211
CTXT_SIZE = 212
CLOSURE_VALUE_NO_CONTEXT_SWITCH = 221
CLOSURE_VALUE_NO_CONTEXT_SWITCH_ = 222
# ___________________________________________________________________________
# Drawing
IDLE_FOR_MICROSECONDS = 230
FORCE_DISPLAY_UPDATE = 231
SET_FULL_SCREEN = 233
# ____________________________________________________________________________
# Time Primitives
UTC_MICROSECOND_CLOCK = 240
LOCAL_MICROSECOND_CLOCK = 241
SIGNAL_AT_UTC_MICROSECONDS = 242
UPDATE_TIMEZONE = 243
# ___________________________________________________________________________
# VM implementor primitives
VM_CLEAR_PROFILE = 250
VM_DUMP_PROFILE = 251
VM_START_PROFILING = 252
VM_STOP_PROFILING = 253
VM_PARAMETERS = 254
META_PRIM_FAILED = 255  # Used to be INST_VARS_PUT_FROM_STACK. Never used except in Disney tests.  Remove after 2.3 release.
# ___________________________________________________________________________
# Quick Push Const Primitives
PUSH_SELF = 256
PUSH_TRUE = 257
PUSH_FALSE = 258
PUSH_NIL = 259
PUSH_MINUS_ONE = 260
PUSH_ZERO = 261
PUSH_ONE = 262
PUSH_TWO = 263
# ___________________________________________________________________________
# VM primitives
VM_LOADED_MODULES = 573
