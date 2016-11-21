from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.primitives import assert_pointers

ImmutabilityPlugin = Plugin()


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object, list, list])
def newImmutableObject(interp, s_frame, w_cls, w_selector_list, w_args_list):
    w_cls = assert_pointers(w_cls)
    s_class = w_cls.as_class_get_shadow(interp.space)
    try:
        new_obj = s_class.new()
    except MemoryError:
        raise PrimitiveFailedError
    return _fill(interp, new_obj, w_selector_list, w_args_list)


def _fill(interp, new_obj, w_selector_list, w_args_list):
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
        interp.perform(new_obj, w_selector=w_selector, w_arguments=w_arguments)
    new_obj.set_immutable(True)
    return new_obj


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def isImmutable(interp, s_frame, w_recv):
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false
