from spyvm import model, constants
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

    def activate(self, w_current_frame):
        sched = scheduler(self.space)
        sched.store_active_process(self._w_self)
        w_frame = self.suspended_context()
        self.store_suspended_context(self.space.w_nil)
        self.store_my_list(self.space.w_nil)
        return w_frame

    def deactivate(self, w_current_frame):
        self.put_to_sleep()
        self.store_suspended_context(w_current_frame)

    def resume(self, w_current_frame):
        sched = scheduler(self.space)
        active_process = ProcessWrapper(self.space, sched.active_process())
        active_priority = active_process.priority()
        priority = self.priority()
        if priority > active_priority:
            active_process.deactivate(w_current_frame)
            return self.activate(w_current_frame)
        else:
            self.put_to_sleep()
            return w_current_frame

    def is_active_process(self):
        return self._w_self.is_same_object(scheduler(self.space).active_process())

    def suspend(self, w_current_frame):
        if self.is_active_process():
            assert self.my_list().is_same_object(self.space.w_nil)
            w_process = scheduler(self.space).highest_priority_process()
            return ProcessWrapper(self.space, w_process).activate(w_current_frame)
        else:
            process_list = ProcessListWrapper(self.space, self.my_list())
            process_list.remove(self._w_self)
            self.store_my_list(self.space.w_nil)
            return w_current_frame

class LinkedListWrapper(Wrapper):
    first_link, store_first_link = make_getter_setter(0)
    last_link, store_last_link = make_getter_setter(1)

    def is_empty_list(self):
        return self.first_link().is_same_object(self.space.w_nil)

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
            while not w_next.is_same_object(self.space.w_nil):
                if w_next.is_same_object(w_link):
                    LinkWrapper(self.space, w_link).store_next_link(self.space.w_nil)
                    w_tail = LinkWrapper(self.space, w_next).next_link()
                    current.store_next_link(w_tail)
                    if w_tail.is_same_object(self.space.w_nil):
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

    def highest_priority_process(self):
        w_lists = self.priority_list()
        # Asserts as W_PointersObjectonion in the varnish. 
        lists = Wrapper(self.space, w_lists)
        
        for i in range(w_lists.size() - 1, -1, -1):
            process_list = ProcessListWrapper(self.space, lists.read(i))
            if not process_list.is_empty_list():
                return process_list.remove_first_link_of_list()

        raise FatalError("Scheduler could not find a runnable process")

def scheduler(space):
    w_association = space.objtable["w_schedulerassociationpointer"]
    assert w_association is not None
    w_scheduler = AssociationWrapper(space, w_association).value()
    assert isinstance(w_scheduler, model.W_PointersObject)
    return SchedulerWrapper(space, w_scheduler)

class SemaphoreWrapper(LinkedListWrapper):

    excess_signals, store_excess_signals = make_int_getter_setter(2)

    def signal(self, w_current_frame):
        if self.is_empty_list():
            value = self.excess_signals()
            self.store_excess_signals(value + 1)
            return w_current_frame
        else:
            process = self.remove_first_link_of_list()
            return ProcessWrapper(self.space, process).resume(w_current_frame)

    def wait(self, w_current_frame):
        excess = self.excess_signals()
        w_process = scheduler(self.space).active_process()
        if excess > 0:
            self.store_excess_signals(excess - 1)
            return w_current_frame
        else:
            self.add_last_link(w_process)
            return ProcessWrapper(self.space, w_process).suspend(w_current_frame)

class PointWrapper(Wrapper):
    x, store_x = make_int_getter_setter(0)
    y, store_y = make_int_getter_setter(1)

 
class BlockClosureWrapper(VarsizedWrapper):
    outerContext, store_outerContext = make_getter_setter(constants.BLKCLSR_OUTER_CONTEXT)
    startpc, store_startpc = make_int_getter_setter(constants.BLKCLSR_STARTPC)
    numArgs, store_numArgs = make_int_getter_setter(constants.BLKCLSR_NUMARGS)

    def asContextWithSender(self, w_aContext, arguments):
        from spyvm import shadow
        s_outerContext = self.outerContext().get_shadow(self.space)
        s_method = s_outerContext.w_method().as_compiledmethod_get_shadow(self.space)
        w_receiver = s_outerContext.w_receiver()
        w_new_frame = shadow.MethodContextShadow.make_context(self.space, s_method, w_receiver,
                     arguments, s_sender=w_aContext.get_shadow(self.space), 
                     pc=self.startpc(), closure=self)
        return w_new_frame

    def tempsize(self):
        # We ignore the number of temps a block has, because the first 
        # bytecodes of the block will initialize them for us. We will only 
        # use this information for decinding where the stack pointer should be 
        # initialy.
        # For a finding the correct number, see BlockClosure>#numTemps in an Image.
        return self.size() + self.numArgs()
    
    def size(self):
        return self._w_self.size() - constants.BLKCLSR_SIZE

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
