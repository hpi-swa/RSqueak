from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.primitives import assert_pointers

ImmutabilityPlugin = Plugin()


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object, list, list],
                                     clean_stack=False)
def newImmutableObject(interp, s_frame, w_cls, w_selector_list, w_args_list):
    from rsqueakvm.interpreter import LocalReturn
    s_frame.pop_n(2)  # removing our arguments
    w_cls = assert_pointers(w_cls)
    s_class = w_cls.as_class_get_shadow(interp.space)
    try:
        new_obj = s_class.new()
    except MemoryError:
        raise PrimitiveFailedError
    # Fill immutable object
    selector_iter = iter(w_selector_list)
    args_iter = iter(w_args_list)
    # import pdb; pdb.set_trace()
    while True:
        try:
            w_selector = next(selector_iter)
            w_arguments = interp.space.unwrap_array(next(args_iter))
        except StopIteration:
            break
        try:
            s_frame._sendSelector(
                w_selector, len(w_arguments), interp, new_obj,
                new_obj.class_shadow(interp.space), w_arguments=w_arguments)
        except LocalReturn as lr:
            s_frame.push(lr.value(interp.space))
    new_obj.set_immutable(True)
    return new_obj


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def isImmutable(interp, s_frame, w_recv):
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false
