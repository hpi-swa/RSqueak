#!/usr/bin/env python
import py, operator
from collections import OrderedDict
from rsqueakvm.test.test_primitives import MockFrame
from .util import read_image, copy_to_module, cleanup_module, create_space, slow_test

pytestmark = slow_test


def setup_module():
    space, interp, _, _ = read_image('Squeak4.3.image')
    w = space.w
    perform = interp.perform
    copy_to_module(locals(), __name__)
    interp.trace = False

def teardown_module():
    cleanup_module(__name__)

def perform_primitive(rcvr, w_selector, *args):
    code = rcvr.class_shadow(space).lookup(w_selector).primitive()
    assert code
    func = primitives.prim_holder.prim_table[code]
    s_frame = MockFrame(space, [rcvr] + list(args)).as_context_get_shadow(space)
    func(interp, s_frame, len(args))
    return s_frame.pop()

def _op_or_overflow(op, *args):
        try:
            return getattr(operator, op)(*args)
        except OverflowError:
            return False

_known_seletors = {}
def sel(selector):
    def _sel(selector):
        try:
            return space.get_special_selector(selector)
        except Exception:
            s_cls = space.wrap_int(0).getclass(space).as_class_get_shadow(space)
            while True:
                s_super = s_cls.s_superclass()
                w_selector = space.find_symbol_in_methoddict(
                    selector, s_cls, fail=s_super is None)  # raises if appropriate
                if w_selector:
                    return w_selector
                s_cls = s_cls.s_superclass()
    w_selector = _known_seletors.get(selector, None)
    if w_selector is None:
        w_selector = _sel(selector)
        _known_seletors[selector] = w_selector
    return w_selector

the_positive_siege = range(1, 17) + [
    31, 32,
    127, 128,
    255, 256,
    2047, 2048,
    4095, 4096,
    32767, 32768,
    65535, 65536,
    8388607, 8388608,
    134217727, 134217728,
    1073741823, 1073741824,
    2147483647, 2147483648,
    4294967295, 4294967296, #!
    8796093022207, 8796093022208,
    140737488355327, 140737488355328,
    9223372036854775807, 9223372036854775808,
    18446744073709551614, 18446744073709551615,
    36893488147419103231, 36893488147419103232, #!
    1267650600228229401496703205375,
        1267650600228229401496703205376,
    170141183460469231731687303715884105727,
        170141183460469231731687303715884105728,
    340282366920938463463374607431768211455,
        340282366920938463463374607431768211456,
    680564733841876926926749214863536422911,
        680564733841876926926749214863536422912,
] # that'll do

# include all the negatives, too
the_siege = list(reduce(operator.concat,
                        zip(the_positive_siege, map(lambda x: 0 - x,
                                                    the_positive_siege))))

# only a few of them
the_small_siege = the_positive_siege[:len(the_positive_siege)/2]
the_tiny_siege = the_positive_siege[: 2 * len(the_positive_siege) / 7]


# I'm paranoid
assert sum(the_siege) == 0

ops = {
    'unary': {'abs': 'abs', 'inv': ('bitInvert', 'inv'), 'negated': ('negated', 'neg')},
    'binary': {
        'plus': ('+', 'add'),
        'minus': ('-', 'sub'),
        'bitAnd': ('bitAnd:', 'and_'),
        'bitOr': ('bitOr:', 'or_'),
        'bitXor': ('bitXor:', 'xor'),
    },
    'large': {
        'div': ('//', 'floordiv'), #for the integer versions at least
        # 'truediv': ('/', 'truediv'), # would need an #asFloat at the end
        'mod': (r'\\', 'mod'),
        'mul': ('*', 'mul'),
    },
    'compare': {
        'equals': ('=', 'eq'),
        'not_less_than': ('>=', 'ge'),
        'greater_than': ('>', 'gt'),
        'not_greater_than': ('<=', 'le'),
        'less_than': ('<', 'lt'),
        'different_from': ('~=', 'ne'),
    },
    'shift': {'lshift': ('<<', 'lshift'), 'rshift': ('>>', 'rshift')},
    'big': {'raisedTo': ('raisedTo:', 'pow')},
}


@py.test.mark.parametrize('operand', the_siege)
@py.test.mark.parametrize('op', ops['unary'].values(), ids=ops['unary'].keys())
def test_unary(op, operand):
    if isinstance(op, tuple):
        st_op, py_op = op
    else:
        st_op = py_op = op
    assert perform(w(operand), w_selector=sel(st_op)).unwrap_long_untranslated(space) == getattr(operator, py_op)(operand)

@py.test.mark.parametrize('operand1', the_siege)
@py.test.mark.parametrize('op', ops['binary'].values(), ids=ops['binary'].keys())
def test_binary(op, operand1):
    if isinstance(op, tuple):
        st_op, py_op = op
    else:
        st_op = py_op = op
    for operand2 in the_small_siege:
        assert perform(w(operand1), w_selector=sel(st_op), w_arguments=[w(operand2)]).unwrap_long_untranslated(space) == getattr(operator, py_op)(operand1, operand2)

@py.test.mark.parametrize('operand1', the_small_siege)
@py.test.mark.parametrize('op', ops['large'].values(), ids=ops['large'].keys())
def test_large(op, operand1):
    if isinstance(op, tuple):
        st_op, py_op = op
    else:
        st_op = py_op = op
    for operand2 in the_small_siege:
        assert perform(w(operand1), w_selector=sel(st_op), w_arguments=[w(operand2)]).unwrap_long_untranslated(space) == getattr(operator, py_op)(operand1, operand2)

@py.test.mark.parametrize('operand1', the_siege)
@py.test.mark.parametrize('op', ops['compare'].values(), ids=ops['compare'].keys())
def test_compare(op, operand1):
    if isinstance(op, tuple):
        st_op, py_op = op
    else:
        st_op = py_op = op
    for operand2 in the_small_siege:
        assert (perform(w(operand1), w_selector=sel(st_op), w_arguments=[w(operand2)]) is space.w_true) == getattr(operator, py_op)(operand1, operand2)

@py.test.mark.parametrize('operand1', the_small_siege)
@py.test.mark.parametrize('op', ops['shift'].values(), ids=ops['shift'].keys())
def test_shift(op, operand1):
    if isinstance(op, tuple):
        st_op, py_op = op
    else:
        st_op = py_op = op
    for operand2 in the_tiny_siege:
        assert perform(w(operand1), w_selector=sel(st_op), w_arguments=[w(operand2)]).unwrap_long_untranslated(space) == getattr(operator, py_op)(operand1, operand2)


@py.test.mark.parametrize('operand1', the_tiny_siege)
@py.test.mark.parametrize('op', ops['big'].values(), ids=ops['big'].keys())
def test_big(op, operand1):
    if isinstance(op, tuple):
        st_op, py_op = op
    else:
        st_op = py_op = op
    for operand2 in the_tiny_siege:
        assert perform(w(operand1), w_selector=sel(st_op), w_arguments=[w(operand2)]).unwrap_long_untranslated(space) == getattr(operator, py_op)(operand1, operand2)
