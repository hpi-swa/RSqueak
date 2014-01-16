from spyvm import model, shadow, constants, interpreter, objspace
from spyvm.plugins import bitblt

space = objspace.ObjSpace()

# copy from test_miniimage
def w(any):
    # XXX could put this on the space?
    if any is None:
        return space.w_nil
    if isinstance(any, str):
        # assume never have strings of length 1
        if len(any) == 1:
            return space.wrap_chr(any)
        else:
            return space.wrap_string(any)
    if isinstance(any, bool):
        return space.wrap_bool(any)
    if isinstance(any, int):
        return space.wrap_int(any)
    if isinstance(any, float):
        return space.wrap_float(any)
    else:
        raise Exception

def make_form(bits, width, height, depth, o_x=0, o_y=0):
    w_f = model.W_PointersObject(space, space.w_Array, 5)
    w_f.store(space, 0, model.W_WordsObject(space, space.w_Array, len(bits)))
    w_f.fetch(space, 0).words = bits
    w_f.store(space, 1, w(width))
    w_f.store(space, 2, w(height))
    w_f.store(space, 3, w(depth))
    w_f.store(space, 4, model.W_PointersObject(space, space.w_Point, 2))
    w_f.fetch(space, 4).store(space, 0, w(o_x))
    w_f.fetch(space, 4).store(space, 1, w(o_y))
    return w_f

def test_bitBlt_values():

    w_bb = model.W_PointersObject(space, space.w_Array, 15)
    w_bb.store(space, 0, make_form([0] * 1230 * 20, 1230, 20, 1))
    w_bb.store(space, 1, w_bb.fetch(space, 0))

    w_bb.store(space, 2, space.w_nil)
    w_bb.store(space, 3, w(7))     # combination rule
    w_bb.store(space, 4, w(1))     # dest x
    w_bb.store(space, 5, w(0))     # dest y
    w_bb.store(space, 6, w(1220))  # width
    w_bb.store(space, 7, w(15))    # height
    w_bb.store(space, 8, w(0))     # source x
    w_bb.store(space, 9, w(0))     # source y
    w_bb.store(space, 10, w(0))    # clip x
    w_bb.store(space, 11, w(0))    # clip y
    w_bb.store(space, 12, w(1220)) # clip width
    w_bb.store(space, 13, w(15))   # clip height
    w_bb.store(space, 14, model.W_PointersObject(space, space.w_Array, 5)) # color map

    s_bb = w_bb.as_special_get_shadow(space, bitblt.BitBltShadow)
    s_bb.loadBitBlt()
    s_bb.clipRange()
    assert not (s_bb.width <= 0 or s_bb.height <= 0)
    s_bb.destMaskAndPointerInit()
    s_bb.checkSourceOverlap()
    s_bb.sourceSkewAndPointerInit()

    assert s_bb.destX == 1
    assert s_bb.destY == 0
    assert s_bb.sourceX == 0
    assert s_bb.sourceY == 0
    assert s_bb.destX == 1
    assert s_bb.destY == 0
    assert s_bb.width == 1220
    assert s_bb.height == 15
    assert s_bb.hDir == -1
    assert s_bb.vDir == 1
    assert s_bb.sourceDelta == 79
    assert s_bb.destDelta == 78
    assert s_bb.skew == 31
    assert s_bb.sourceIndex == 38
    assert s_bb.destIndex == 38
