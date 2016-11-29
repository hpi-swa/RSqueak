
from rsqueakvm.interpreter import Interpreter, jit_driver_name
from rsqueakvm.model.compiled_methods import W_CompiledMethod

from rpython.rtyper.lltypesystem import lltype
from rpython.rtyper.annlowlevel import cast_base_ptr_to_instance
from rpython.rtyper.rclass import OBJECT
from rpython.rlib.objectmodel import compute_unique_id


def _assoc(space, k, w_v):
    from rsqueakvm.wrapper import AssociationWrapper
    return AssociationWrapper.make_w_assoc(space, space.wrap_string(k), w_v)

def wrap_greenkey(space, jitdriver, greenkey, greenkey_repr):
    if greenkey is None:
        return space.w_nil
    jitdriver_name = jitdriver.name
    if jitdriver_name == jit_driver_name:
        pc = greenkey[0].getint()
        w_method = method_from_greenkey(space, jitdriver, greenkey)
        assert w_method is not None
        return space.wrap_list([_assoc(space, "pc", space.wrap_int(pc)), _assoc(space, "method", w_method)])
    else:
        # return space.wrap_string(greenkey_repr)
        return space.w_nil

def method_from_greenkey(space, jitdriver, greenkey):
    if greenkey is None:
        return space.w_nil
    jitdriver_name = jitdriver.name
    if jitdriver_name == jit_driver_name:
        ll_method = lltype.cast_opaque_ptr(lltype.Ptr(OBJECT), greenkey[2].getref_base())
        w_method = cast_base_ptr_to_instance(W_CompiledMethod, ll_method)
        return w_method
    else:
        return space.w_nil

def wrap_oplist(space, logops, operations, ops_offset=None):
    from rpython.jit.metainterp.resoperation import rop
    l_w = []
    jitdrivers_sd = logops.metainterp_sd.jitdrivers_sd
    for op in operations:
        if ops_offset is None:
            ofs = -1
        else:
            ofs = ops_offset.get(op, 0)
        num = op.getopnum()
        name = op.getopname()
        if num == rop.DEBUG_MERGE_POINT:
            jd_sd = jitdrivers_sd[op.getarg(0).getint()]
            greenkey = op.getarglist()[3:]
            repr = jd_sd.warmstate.get_location_str(greenkey)
            w_greenkey = wrap_greenkey(space, jd_sd.jitdriver, greenkey, repr)
            l_w.append(space.wrap_list(DebugMergePoint(space, name,
                                                       logops.repr_of_resop(op),
                                                       jd_sd.jitdriver.name,
                                                       op.getarg(1).getint(),
                                                       op.getarg(2).getint(),
                                                       w_greenkey)))
        elif op.is_guard():
            l_w.append(space.wrap_list(GuardOp(space, name, ofs, logops.repr_of_resop(op),
                                               op.getdescr().get_jitcounter_hash())))
        else:
            l_w.append(space.wrap_list(WrappedOp(space, name, ofs, logops.repr_of_resop(op))))
    return space.wrap_list(l_w)

def WrappedOp(space, name, offset, repr_of_resop):
    return [
        _assoc(space, "name", space.wrap_string(name)),
        _assoc(space, "offset", space.wrap_int(offset)),
        _assoc(space, "repr_of_resop", space.wrap_string(repr_of_resop))]

def GuardOp(space, name, offset, repr_of_resop, hash):
    lst = WrappedOp(space, name, offset, repr_of_resop)
    lst.append(_assoc(space, "hash", space.wrap_int(hash)))
    return lst

def DebugMergePoint(space, name, repr_of_resop, jd_name, call_depth, call_id, w_greenkey):
    lst = WrappedOp(space, name, -1, repr_of_resop)
    lst.extend([
        _assoc(space, "jd_name", space.wrap_string(jd_name)),
        _assoc(space, "call_depth", space.wrap_int(call_depth)),
        _assoc(space, "call_id", space.wrap_int(call_id)),
        _assoc(space, "greenkey", w_greenkey)])
    return lst

def wrap_debug_info(space, debug_info, is_bridge=False):
    memo = {}
    logops = debug_info.logger._make_log_operations(memo)
    if debug_info.asminfo is not None:
        ofs = debug_info.asminfo.ops_offset
    else:
        ofs = {}
    ops = debug_info.operations
    w_ops = wrap_oplist(space, logops, ops, ofs)

    jd_name = debug_info.get_jitdriver().name
    type = debug_info.type
    bridge_no = -1
    w_green_key = space.w_nil

    if is_bridge:
        bridge_no = compute_unique_id(debug_info.fail_descr)
    else:
        w_green_key = wrap_greenkey(space,
                                    debug_info.get_jitdriver(),
                                    debug_info.greenkey,
                                    debug_info.get_greenkey_repr())
    loop_no = debug_info.looptoken.number
    asminfo = debug_info.asminfo
    asmaddr = space.w_nil
    asmlen = space.w_nil
    if asminfo is not None:
        asmaddr = space.wrap_int(asminfo.asmaddr)
        asmlen = space.wrap_int(asminfo.asmlen)
    return space.wrap_list([
        _assoc(space, "type", space.wrap_string(type)),
        _assoc(space, "ops", w_ops),
        _assoc(space, "jd_name", space.wrap_string(jd_name)),
        _assoc(space, "bridge no", space.wrap_int(bridge_no)),
        _assoc(space, "greenkey", w_green_key),
        _assoc(space, "loop no", space.wrap_int(loop_no)),
        _assoc(space, "asmaddr", asmaddr),
        _assoc(space, "asmlen", asmlen)])
