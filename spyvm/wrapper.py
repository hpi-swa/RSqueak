from spyvm import model, model_display, constants
from spyvm.error import FatalError, WrapperException, PrimitiveFailedError

class Wrapper(object):
    def __init__(self, space, w_self):
        if not isinstance(w_self, model.W_PointersObject):
            raise WrapperException("Unexpected instance given to wrapper")
        self._w_self = w_self
        self.space = space

    def read(self, index0):
        try:
            return self._w_self.fetch(self.space, index0)
            # XXX Index error never raised after translation
        except IndexError:
            raise WrapperException("Unexpected instance layout. Too small")

    def write(self, index0, w_new):
        try:
            self._w_self.store(self.space, index0, w_new)
            # XXX Index error never raised after translation
        except IndexError:
            raise WrapperException("Unexpected instance layout. Too small")

class VarsizedWrapper(Wrapper):
    def at0(self, i0):
        return self._w_self.at0(self.space, i0)

    def atput0(self, i0, w_value):
        return self._w_self.atput0(self.space, i0, w_value)


def make_getter(index0):
    def getter(self):
        return self.read(index0)
    return getter

def make_setter(index0):
    def setter(self, w_new):
        return self.write(index0, w_new)
    return setter

def make_getter_setter(index0):
    return make_getter(index0), make_setter(index0)

def make_int_getter(index0):
    def getter(self):
        return self.space.unwrap_int(self.read(index0))
    return getter

def make_int_setter(index0):
    def setter(self, new):
        return self.write(index0, self.space.wrap_int(new))
    return setter

def make_int_getter_setter(index0):
    return make_int_getter(index0), make_int_setter(index0)


class LinkWrapper(Wrapper):
    next_link, store_next_link = make_getter_setter(0)

class ProcessWrapper(LinkWrapper):
    suspended_context, store_suspended_context = make_getter_setter(1)
    priority = make_int_getter(2)
    my_list, store_my_list = make_getter_setter(3)

    def put_to_sleep(self):
        sched = scheduler(self.space)
        priority = self.priority()
        process_list = sched.get_process_list(priority)
        process_list.add_process(self._w_self)

    def activate(self):
        from spyvm.interpreter import ProcessSwitch
        assert not self.is_active_process()
        sched = scheduler(self.space)
        sched.store_active_process(self._w_self)
        w_frame = self.suspended_context()
        self.store_suspended_context(self.space.w_nil)
        self.store_my_list(self.space.w_nil)
        assert isinstance(w_frame, model.W_PointersObject)
        raise ProcessSwitch(w_frame.as_context_get_shadow(self.space))

    def deactivate(self, s_current_frame, put_to_sleep=True):
        if put_to_sleep:
            self.put_to_sleep()
        self.store_suspended_context(s_current_frame.w_self())

    def resume(self, s_current_frame):
        sched = scheduler(self.space)
        active_process = ProcessWrapper(self.space, sched.active_process())
        active_priority = active_process.priority()
        priority = self.priority()
        if priority > active_priority:
            if not self.space.suppress_process_switch.is_set():
                active_process.deactivate(s_current_frame)
                self.activate()
        else:
            self.put_to_sleep()

    def is_active_process(self):
        return self._w_self.is_same_object(scheduler(self.space).active_process())

    def suspend(self, s_current_frame):
        if self.is_active_process():
            if not self.space.suppress_process_switch.is_set():
                assert self.my_list().is_nil(self.space)
                w_process = scheduler(self.space).pop_highest_priority_process()
                self.deactivate(s_current_frame, put_to_sleep=False)
                ProcessWrapper(self.space, w_process).activate()
        else:
            if not self.my_list().is_nil(self.space):
                process_list = ProcessListWrapper(self.space, self.my_list())
                process_list.remove(self._w_self)
                self.store_my_list(self.space.w_nil)

class LinkedListWrapper(Wrapper):
    first_link, store_first_link = make_getter_setter(0)
    last_link, store_last_link = make_getter_setter(1)

    def is_empty_list(self):
        return self.first_link().is_nil(self.space)

    def add_last_link(self, w_object):
        if self.is_empty_list():
            self.store_first_link(w_object)
        else:
            LinkWrapper(self.space, self.last_link()).store_next_link(w_object)
        self.store_last_link(w_object)

    def remove_first_link_of_list(self):
        w_first = self.first_link()
        w_last = self.last_link()
        if w_first.is_same_object(w_last):
            self.store_first_link(self.space.w_nil)
            self.store_last_link(self.space.w_nil)
        else:
            w_next = LinkWrapper(self.space, w_first).next_link()
            self.store_first_link(w_next)
        LinkWrapper(self.space, w_first).store_next_link(self.space.w_nil)
        return w_first

    def remove(self, w_link):
        if self.first_link().is_same_object(w_link):
            self.remove_first_link_of_list()
            return
        else:
            current = LinkWrapper(self.space, self.first_link())
            w_next = current.next_link()
            while not w_next.is_nil(self.space):
                if w_next.is_same_object(w_link):
                    LinkWrapper(self.space, w_link).store_next_link(self.space.w_nil)
                    w_tail = LinkWrapper(self.space, w_next).next_link()
                    current.store_next_link(w_tail)
                    if w_tail.is_nil(self.space):
                        self.store_last_link(current._w_self)
                    return
                current = LinkWrapper(self.space, w_next)
                w_next = current.next_link()
        raise WrapperException("Could not find link")

class ProcessListWrapper(LinkedListWrapper):
    def add_process(self, w_process):
        self.add_last_link(w_process)
        ProcessWrapper(self.space, w_process).store_my_list(self._w_self)

class AssociationWrapper(Wrapper):
    key = make_getter(0)
    value, store_value = make_getter_setter(1)

class SchedulerWrapper(Wrapper):
    priority_list = make_getter(0)
    active_process, store_active_process = make_getter_setter(1)

    def get_process_list(self, priority):
        lists = Wrapper(self.space, self.priority_list())

        return ProcessListWrapper(self.space, lists.read(priority))

    def pop_highest_priority_process(self):
        w_lists = self.priority_list()
        # Asserts as W_PointersObjectonion in the varnish.
        lists = Wrapper(self.space, w_lists)

        for i in range(w_lists.size() - 1, -1, -1):
            process_list = ProcessListWrapper(self.space, lists.read(i))
            if not process_list.is_empty_list():
                return process_list.remove_first_link_of_list()

        raise FatalError("Scheduler could not find a runnable process")

    def highest_priority_process(self):
        w_lists = self.priority_list()
        # Asserts as W_PointersObjectonion in the varnish.
        lists = Wrapper(self.space, w_lists)

        for i in range(w_lists.size() - 1, -1, -1):
            process_list = ProcessListWrapper(self.space, lists.read(i))
            if not process_list.is_empty_list():
                return process_list.first_link()

        raise FatalError("Scheduler could not find a runnable process")

def scheduler(space):
    w_association = space.objtable["w_schedulerassociationpointer"]
    assert w_association is not None
    w_scheduler = AssociationWrapper(space, w_association).value()
    assert isinstance(w_scheduler, model.W_PointersObject)
    return SchedulerWrapper(space, w_scheduler)

class SemaphoreWrapper(LinkedListWrapper):

    excess_signals, store_excess_signals = make_int_getter_setter(2)

    def signal(self, s_current_frame):
        if self.is_empty_list():
            value = self.excess_signals()
            self.store_excess_signals(value + 1)
        else:
            process = self.remove_first_link_of_list()
            ProcessWrapper(self.space, process).resume(s_current_frame)

    def wait(self, s_current_frame):
        excess = self.excess_signals()
        w_process = scheduler(self.space).active_process()
        if excess > 0:
            self.store_excess_signals(excess - 1)
        else:
            self.add_last_link(w_process)
            ProcessWrapper(self.space, w_process).suspend(s_current_frame)

class PointWrapper(Wrapper):
    x, store_x = make_int_getter_setter(0)
    y, store_y = make_int_getter_setter(1)


class BlockClosureWrapper(VarsizedWrapper):
    outerContext, store_outerContext = make_getter_setter(constants.BLKCLSR_OUTER_CONTEXT)
    startpc, store_startpc = make_int_getter_setter(constants.BLKCLSR_STARTPC)
    numArgs, store_numArgs = make_int_getter_setter(constants.BLKCLSR_NUMARGS)

    def create_frame(self, arguments=[]):
        from spyvm import storage_contexts
        w_outerContext = self.outerContext()
        if not isinstance(w_outerContext, model.W_PointersObject):
            raise PrimitiveFailedError
        s_outerContext = w_outerContext.as_context_get_shadow(self.space)
        w_method = s_outerContext.w_method()
        w_receiver = s_outerContext.w_receiver()
        return storage_contexts.ContextPartShadow.build_method_context(self.space, w_method, w_receiver, arguments, self)

    def tempsize(self):
        # We ignore the number of temps a block has, because the first
        # bytecodes of the block will initialize them for us. We will only
        # use this information for deciding where the stack pointer should be
        # initialy.
        # For a finding the correct number, see BlockClosure>#numTemps in an Image.
        return self.size() + self.numArgs()

    def size(self):
        return self._w_self.size() - constants.BLKCLSR_SIZE

class FormWrapper(Wrapper):
    bits, store_bits = make_getter_setter(constants.FORM_BITS)
    width, store_width = make_int_getter_setter(constants.FORM_WIDTH)
    height, store_height = make_int_getter_setter(constants.FORM_HEIGHT)
    depth, store_depth = make_int_getter_setter(constants.FORM_DEPTH)

    def create_display_bitmap(self):
        w_display_bitmap = model_display.from_words_object(self.bits(), self)
        self.store_bits(w_display_bitmap)
        return w_display_bitmap

    def get_display_bitmap(self):
        w_bitmap = self.bits()
        if not isinstance(w_bitmap, model_display.W_DisplayBitmap):
            w_display_bitmap = self.create_display_bitmap()
        else:
            w_display_bitmap = w_bitmap
            if w_display_bitmap._depth != self.depth():
                w_display_bitmap = self.create_display_bitmap()
        return w_display_bitmap

    def take_over_display(self):
        self.space.display().set_video_mode(self.width(), self.height(), self.depth())

# XXX Wrappers below are not used yet.
class OffsetWrapper(Wrapper):
    offset_x  = make_int_getter(0)
    offset_y  = make_int_setter(1)

class MaskWrapper(Wrapper):
    bits       = make_getter(0)
    extend_x   = make_int_getter(1)
    extend_y   = make_int_getter(2)
    depth      = make_int_getter(3)

class CursorWrapper(MaskWrapper):
    offset   = make_getter(4)
