from spyvm import model, shadow, constants, interpreter, objspace

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
    w_bb.store(space, 0, make_form([], 1230, 20, 1))
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

    s_bb = w_bb.as_bitblt_get_shadow(space)
    s_bb.clip_range()
    assert not (s_bb.w <= 0 or s_bb.h <= 0)
    s_bb.compute_masks()
    s_bb.check_overlap()
    s_bb.calculate_offsets()

    assert s_bb.dest_x == 1
    assert s_bb.dest_y == 0
    assert s_bb.sx == 1218
    assert s_bb.sy == 0
    assert s_bb.dx == 1219
    assert s_bb.dy == 0
    assert s_bb.w == 1219
    assert s_bb.h == 15
    assert s_bb.h_dir == -1
    assert s_bb.v_dir == 1
    assert s_bb.source_delta == 79
    assert s_bb.dest_delta == 78
    assert s_bb.skew == 31
    assert s_bb.source_index == 38
    assert s_bb.dest_index == 38