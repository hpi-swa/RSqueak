import py
from spyvm import wrapper, model, interpreter, objspace
from spyvm.error import WrapperException, FatalError
from .util import create_space, copy_to_module, cleanup_module

def setup_module():
    space = create_space(bootstrap = True)
    new_frame = lambda: space.make_frame("")[1]
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def test_simpleread():
    w_o = model.W_PointersObject(space, None, 2)
    w = wrapper.Wrapper(space, w_o)
    w_o.store(space, 0, "hello")
    assert w.read(0) == "hello"
    w.write(1, "b")
    assert w.read(1) == "b"
    py.test.raises(WrapperException, "w.read(2)")
    py.test.raises(WrapperException, "w.write(2, \"test\")")

def test_accessor_generators():
    w_o = model.W_PointersObject(space, None, 1)
    w = wrapper.LinkWrapper(space, w_o)
    w_o.store(space, 0, "hello")
    assert w.next_link() == "hello"
    w.store_next_link("boe")
    assert w.next_link() == "boe"

def link(w_next='foo'):
    w_object = model.W_PointersObject(space, None, 1)
    wrapper.LinkWrapper(space, w_object).store_next_link(w_next)
    return w_object

def test_linked_list():
    w_object = model.W_PointersObject(space, None,2)
    w_last = link(space.w_nil)
    w_lb1 = link(w_last)
    w_lb2 = link(w_lb1)
    w_lb3 = link(w_lb2)
    w_lb4 = link(w_lb3)
    w_first = link(w_lb4)
    linkedlist = wrapper.LinkedListWrapper(space, w_object)
    linkedlist.store_first_link(w_first)
    linkedlist.store_last_link(w_last)
    assert w_first is linkedlist.first_link()
    assert w_last is linkedlist.last_link()
    assert linkedlist.remove_first_link_of_list() is w_first
    assert linkedlist.remove_first_link_of_list() is w_lb4
    assert linkedlist.remove_first_link_of_list() is w_lb3
    assert not linkedlist.is_empty_list()
    assert linkedlist.remove_first_link_of_list() is w_lb2
    assert linkedlist.remove_first_link_of_list() is w_lb1
    assert linkedlist.remove_first_link_of_list() is w_last
    assert linkedlist.is_empty_list()
    linkedlist.add_last_link(w_first)
    assert linkedlist.first_link() is w_first
    assert linkedlist.last_link() is w_first
    linkedlist.add_last_link(w_last)
    assert linkedlist.first_link() is w_first
    assert linkedlist.last_link() is w_last
    py.test.raises(WrapperException, linkedlist.remove, space.w_nil)
    linkedlist.remove(w_first)
    assert linkedlist.first_link() is w_last
    linkedlist.store_first_link(w_first)
    wrapper.LinkWrapper(space, w_first).store_next_link(w_last)
    linkedlist.remove(w_last)
    assert linkedlist.last_link() is w_first

def new_process(w_next=None,
                w_my_list=None,
                w_suspended_context=None,
                priority=0):
    if w_next is None:
        w_next = space.w_nil
    if w_my_list is None:
        w_my_list = space.w_nil
    if w_suspended_context is None:
        w_suspended_context = space.w_nil
    w_priority = space.wrap_int(priority)
    w_process = model.W_PointersObject(space, None, 4)
    process = wrapper.ProcessWrapper(space, w_process)
    process.store_next_link(w_next)
    process.store_my_list(w_my_list)
    process.store_suspended_context(w_suspended_context)
    process.write(2, w_priority)
    return process

def new_processlist(processes_w=[]):
    w_processlist = model.W_PointersObject(space, None, 2)
    w_first = space.w_nil
    w_last = space.w_nil
    for w_process in processes_w[::-1]:
        w_first = newprocess(w_first, w_processlist).wrapped
        if w_last.is_nil(space):
            w_last = w_first
    pl = wrapper.ProcessListWrapper(space, w_processlist)
    pl.store_first_link(w_first)
    pl.store_last_link(w_last)
    return pl

def new_prioritylist(prioritydict=None):
    if prioritydict is not None:
        maxpriority = max(prioritydict.keys())
    else:
        maxpriority = 5
        prioritydict = {}
    w_prioritylist = model.W_PointersObject(space, None, maxpriority)
    prioritylist = wrapper.Wrapper(space, w_prioritylist)
    for i in range(maxpriority):
        prioritylist.write(i, new_processlist(prioritydict.get(i, [])).wrapped)

    return prioritylist

def new_scheduler(w_process=None, prioritydict=None):
    if w_process is None:
        w_process = space.w_nil
    priority_list = new_prioritylist(prioritydict)
    w_scheduler = model.W_PointersObject(space, None, 2)
    scheduler = wrapper.SchedulerWrapper(space, w_scheduler)
    scheduler.store_active_process(w_process)
    scheduler.write(0, priority_list.wrapped)
    return scheduler

def new_semaphore(excess_signals=0):
    w_semaphore = model.W_PointersObject(space, None, 3)
    semaphore = wrapper.SemaphoreWrapper(space, w_semaphore)
    semaphore.store_excess_signals(excess_signals)
    return semaphore


class TestScheduler(object):
    def setup_method(self, meth):
        self.old_scheduler = wrapper.scheduler
        wrapper.scheduler = lambda space: scheduler
        scheduler = new_scheduler()

    def teardown_method(self, meth):
        wrapper.scheduler = self.old_scheduler

    def test_put_to_sleep(self):
        process = new_process(priority=2)
        process.put_to_sleep()
        process_list = wrapper.scheduler(space).get_process_list(2)
        assert process_list.first_link() is process_list.last_link()
        assert process_list.first_link() is process.wrapped

    def test_suspend_asleep(self):
        process, old_process = self.make_processes(4, 2, space.w_false)
        process.suspend(space.w_true)
        process_list = wrapper.scheduler(space).get_process_list(process.priority())
        assert process_list.first_link() is process_list.last_link()
        assert process_list.first_link().is_nil(space)
        assert process.my_list().is_nil(space)

    def test_suspend_active(self):
        suspended_context = new_frame()
        process, old_process = self.make_processes(4, 2, suspended_context)
        current_context = new_frame()
        with py.test.raises(interpreter.ProcessSwitch):
            old_process.suspend(current_context)
        process_list = wrapper.scheduler(space).get_process_list(old_process.priority())
        assert process_list.first_link() is process_list.last_link()
        assert process_list.first_link().is_nil(space)
        assert old_process.my_list().is_nil(space)
        assert old_process.suspended_context() is current_context.w_self()
        assert wrapper.scheduler(space).active_process() is process.wrapped

    def new_process_consistency(self, process, old_process, w_active_context):
        scheduler = wrapper.scheduler(space)
        assert scheduler.active_process() is process.wrapped
        priority_list = wrapper.scheduler(space).get_process_list(process.priority())
        assert priority_list.first_link() is priority_list.last_link()
        # activate does not remove the process from the process_list.
        # The caller of activate is responsible
        assert priority_list.first_link() is process.wrapped

    def old_process_consistency(self, old_process, old_process_context):
        assert old_process.suspended_context() is old_process_context.w_self()
        priority_list = wrapper.scheduler(space).get_process_list(old_process.priority())
        assert priority_list.first_link() is old_process.wrapped

    def make_processes(self, sleepingpriority, runningpriority,
                             sleepingcontext):
        if not isinstance(sleepingcontext, model.W_Object):
            sleepingcontext = sleepingcontext.w_self()
        scheduler = wrapper.scheduler(space)
        sleeping = new_process(priority=sleepingpriority, w_suspended_context=sleepingcontext)
        sleeping.put_to_sleep()
        running = new_process(priority=runningpriority)
        scheduler.store_active_process(running.wrapped)

        return sleeping, running


    def test_activate(self):
        sleepingcontext = new_frame()
        process, old_process = self.make_processes(4, 2, sleepingcontext)
        try:
            process.activate()
        except interpreter.ProcessSwitch, e:
            w_frame = e.s_new_context._w_self
        self.new_process_consistency(process, old_process, w_frame)

    def test_resume(self):
        sleepingcontext = new_frame()
        currentcontext = new_frame()
        process, old_process = self.make_processes(4, 2, sleepingcontext)
        try:
            process.resume(currentcontext)
        except interpreter.ProcessSwitch, e:
            w_frame = e.s_new_context._w_self
        self.new_process_consistency(process, old_process, w_frame)
        self.old_process_consistency(old_process, currentcontext)

        # Does not reactivate old_process because lower priority
        w_frame = old_process.resume(w_frame)
        self.new_process_consistency(process, old_process, w_frame)
        self.old_process_consistency(old_process, currentcontext)

    def test_semaphore_excess_signal(self):
        semaphore = new_semaphore()
        self.space = space
        semaphore.signal(self)
        assert semaphore.excess_signals() == 1

    def test_highest_priority(self):
        py.test.raises(FatalError, wrapper.scheduler(space).pop_highest_priority_process)
        process, old_process = self.make_processes(4, 2, space.w_false)
        process.put_to_sleep()
        old_process.put_to_sleep()
        highest = wrapper.scheduler(space).pop_highest_priority_process()
        assert highest is process.wrapped
        highest = wrapper.scheduler(space).pop_highest_priority_process()
        assert highest is old_process.wrapped
        py.test.raises(FatalError, wrapper.scheduler(space).pop_highest_priority_process)

    def test_semaphore_wait(self):
        semaphore = new_semaphore()
        suspendedcontext = new_frame()
        currentcontext = new_frame()
        process, old_process = self.make_processes(4, 2, suspendedcontext)
        with py.test.raises(interpreter.ProcessSwitch):
            semaphore.wait(currentcontext)
        assert semaphore.first_link() is old_process.wrapped
        assert wrapper.scheduler(space).active_process() is process.wrapped

    def test_semaphore_signal_wait(self):
        semaphore = new_semaphore()
        self.space = space
        semaphore.signal(self)
        suspendedcontext = new_frame()
        currentcontext = new_frame()
        process, old_process = self.make_processes(4, 2, suspendedcontext)
        semaphore.wait(currentcontext)
        assert semaphore.is_empty_list()
        assert wrapper.scheduler(space).active_process() is old_process.wrapped
        with py.test.raises(interpreter.ProcessSwitch):
            semaphore.wait(currentcontext)
        assert semaphore.first_link() is old_process.wrapped
        assert wrapper.scheduler(space).active_process() is process.wrapped

        py.test.raises(FatalError, semaphore.wait, space.w_true)

    def test_semaphore_wait_signal(self):
        semaphore = new_semaphore()
        suspendedcontext = new_frame()
        currentcontext = new_frame()
        process, old_process = self.make_processes(4, 2, suspendedcontext)

        with py.test.raises(interpreter.ProcessSwitch):
            semaphore.wait(currentcontext)

        assert wrapper.scheduler(space).active_process() is process.wrapped
        semaphore.signal(currentcontext)
        assert wrapper.scheduler(space).active_process() is process.wrapped
        process_list = wrapper.scheduler(space).get_process_list(old_process.priority())
        assert process_list.remove_first_link_of_list() is old_process.wrapped

        process.write(2, space.wrap_int(1))
        with py.test.raises(interpreter.ProcessSwitch):
            old_process.resume(currentcontext)
        assert wrapper.scheduler(space).active_process() is old_process.wrapped
        with py.test.raises(interpreter.ProcessSwitch):
            semaphore.wait(currentcontext)
        assert wrapper.scheduler(space).active_process() is process.wrapped
        with py.test.raises(interpreter.ProcessSwitch):
            semaphore.signal(currentcontext)
        assert wrapper.scheduler(space).active_process() is old_process.wrapped

        process_list = wrapper.scheduler(space).get_process_list(process.priority())
        assert process_list.first_link() is process.wrapped
