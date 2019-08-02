from rpython.rlib import jit, objectmodel, rgc

from rsqueakvm import constants
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.numeric import W_SmallInteger
from rsqueakvm.primitives import expose_primitive
from rsqueakvm.primitives.constants import *


# ___________________________________________________________________________
# Drawing

@expose_primitive(IDLE_FOR_MICROSECONDS,
                  unwrap_spec=[object, int], no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr, time_mu_s):
    import time
    s_frame.pop()
    time_s = time_mu_s / 1000000.0
    interp.interrupt_check_counter = 0
    interp.quick_check_for_interrupt(s_frame, dec=0)
    time.sleep(time_s)
    interp.interrupt_check_counter = 0
    interp.quick_check_for_interrupt(s_frame, dec=0)

@expose_primitive(FORCE_DISPLAY_UPDATE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    interp.space.display().render(force=True)
    return w_rcvr

@expose_primitive(SET_FULL_SCREEN, unwrap_spec=[object, bool])
def func(interp, s_frame, w_rcvr, flag):
    interp.space.display().set_full_screen(flag)
    return w_rcvr

# ___________________________________________________________________________
# VM implementor primitives

@expose_primitive(META_PRIM_FAILED, unwrap_spec=[object, int], result_is_new_frame=True)
def func(interp, s_frame, w_rcvr, primFailFlag):
    from rsqueakvm.storage_contexts import DirtyContext
    if primFailFlag != 0:
        s_fallback = interp.unwind_primitive_simulation(s_frame, primFailFlag)
        s_fallback.set_state(DirtyContext)
        return s_fallback
    raise PrimitiveFailedError


def translated_or_default(default):
    def decorator(func):
        def wrapped(*args):
            if objectmodel.we_are_translated():
                val = func(*args)
            else:
                val = default
            return val
        wrapped.__name__ = func.__name__
        return jit.dont_look_inside(wrapped)
    return decorator

@translated_or_default(0)
def current_gc_time():
    return rgc.get_stats(rgc.TOTAL_GC_TIME)

@translated_or_default(0)
def current_gc_total_mem():
    return rgc.get_stats(rgc.TOTAL_MEMORY)

@translated_or_default(0)
def current_gc_young_mem():
    return rgc.get_stats(rgc.NURSERY_SIZE)

@translated_or_default(0)
def current_gc_old_mem():
    return rgc.get_stats(rgc.TOTAL_MEMORY) - rgc.get_stats(rgc.NURSERY_SIZE)


@expose_primitive(VM_PARAMETERS)
@jit.dont_look_inside
def func(interp, s_frame, argcount):
    from rpython.rlib import jit_hooks
    """Behaviour depends on argument count:
            0 args: return an Array of VM parameter values;
            1 arg:  return the indicated VM parameter;
            2 args: set the VM indicated parameter.
        VM parameters are numbered as follows:
            1   byte size of old-space (read-only)
            2   byte size of young-space (read-only)
            3   byte size of object memory (read-only)
            4   allocationCount (read-only; nil in Cog VMs)
            5   allocations between GCs (read-write; nil in Cog VMs)
            6   survivor count tenuring threshold (read-write)
            7   full GCs since startup (read-only)
            8   total milliseconds in full GCs since startup (read-only)
            9   incremental GCs since startup (read-only; scavenging GCs on Spur)
            10  total milliseconds in incremental/scavenging GCs since startup (read-only)
            11  tenures of surving objects since startup (read-only)
            12-20 specific to the translating VM (nil in Cog VMs)
            21  root table size (read-only)
            22  root table overflows since startup (read-only)
            23  bytes of extra memory to reserve for VM buffers, plugins, etc.
            24  memory threshold above which to shrink object memory (read-write)
            25  ammount to grow by when growing object memory (read-write)
            26  interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
            27  number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
            28  number of times sweep loop iterated for current IGC/FGC (read-only)
            29  number of times make forward loop iterated for current IGC/FGC (read-only)
            30  number of times compact move loop iterated for current IGC/FGC (read-only)
            31  number of grow memory requests (read-only)
            32  number of shrink memory requests (read-only)
            33  number of root table entries used for current IGC/FGC (read-only)
            34  number of allocations done before current IGC/FGC (read-only)
            35  number of survivor objects after current IGC/FGC (read-only)
            36  millisecond clock when current IGC/FGC completed (read-only)
            37  number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
            38  milliseconds taken by current IGC (read-only)
            39  Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
            40  BytesPerWord for this image
            41  imageFormatVersion for the VM
            42  number of stack pages in use (Cog Stack VM only, otherwise nil)
            43  desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
            44  size of eden, in bytes (Cog VMs only, otherwise nil)
            45  desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
            46  size of machine code zone, in bytes (stored in image file header; Cog JIT VM only, otherwise nil)
            47  desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
            48  various properties of the Cog VM as an integer encoding an array of bit flags.
                Bit 0: implies the image's Process class has threadId as its 3rd inst var (zero relative)
                Bit 1: on Cog VMs asks the VM to set the flag bit in interpreted methods
                Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
                        i.e. preempting a process by a higher one will not cause the process to yield
                            to others at the same priority.
            49  the size of the external semaphore table (read-write; Cog VMs only)
            50-53 reserved for VM parameters that persist in the image (such as eden above)
            54  total size of free old space (Spur only, otherwise nil)
            55  ratio of growth and image size at or above which a GC will be performed post scavenge (Spur only, otherwise nil)
            56  number of process switches since startup (read-only)
            57  number of ioProcessEvents calls since startup (read-only)
            58  number of forceInterruptCheck (Cog VMs) or quickCheckInterruptCalls (non-Cog VMs) calls since startup (read-only)
            59  number of check event calls since startup (read-only)
            60  number of stack page overflows since startup (read-only; Cog VMs only)
            61  number of stack page divorces since startup (read-only; Cog VMs only)
            62  number of machine code zone compactions since startup (read-only; Cog VMs only)
            63  milliseconds taken by machine code zone compactions since startup (read-only; Cog VMs only)
            64  current number of machine code methods (read-only; Cog VMs only)
            65  true if the VM supports multiple bytecode sets;  (read-only; Cog VMs only; nil in older Cog VMs)
            66  the byte size of a stack page in the stack zone  (read-only; Cog VMs only)
            67 - 69 reserved for more Cog-related info
            70  the value of VM_PROXY_MAJOR (the interpreterProxy major version number)
            71  the value of VM_PROXY_MINOR (the interpreterProxy minor version number)

        Note: Thanks to Ian Piumarta for this primitive."""

    if not 0 <= argcount <= 2:
        raise PrimitiveFailedError

    arg1_w = s_frame.pop()  # receiver

    vm_w_params = [interp.space.wrap_int(0)] * 71

    vm_w_params[0] = interp.space.wrap_int(current_gc_old_mem())
    vm_w_params[1] = interp.space.wrap_int(current_gc_young_mem())
    vm_w_params[2] = interp.space.wrap_int(current_gc_total_mem())
    vm_w_params[6] = interp.space.wrap_int(1)
    vm_w_params[7] = interp.space.wrap_int(current_gc_time())
    vm_w_params[8] = interp.space.wrap_int(1)  # must be 1 for VM Stats view to work
    # vm_w_params[9] = interp.space.wrap_int(0)




    vm_w_params[25] = interp.space.wrap_int(interp.interrupt_counter_size) # check for interrupts roughly every N bytecodes

    vm_w_params[41] = interp.space.wrap_int(1)  # We are a "stack-like" VM - number of stack tables

    if objectmodel.we_are_translated():
        # sizeOfMachineCode = jit_hooks.stats_asmmemmgr_allocated(None)
        sizeOfMachineCode = jit_hooks.stats_asmmemmgr_used(None)
        vm_w_params[45] = interp.space.wrap_int(sizeOfMachineCode)

    vm_w_params[39] = interp.space.wrap_int(constants.BYTES_PER_MACHINE_INT)
    vm_w_params[40] = interp.space.wrap_int(interp.image.version.magic)
    vm_w_params[55] = interp.space.wrap_int(interp.process_switch_count)
    vm_w_params[57] = interp.space.wrap_int(interp.forced_interrupt_checks_count)
    vm_w_params[59] = interp.space.wrap_int(interp.stack_overflow_count)

    if objectmodel.we_are_translated():
        numberOfLoops = jit_hooks.stats_get_counter_value(None, jit.Counters.TOTAL_COMPILED_LOOPS)
        numberOfBridges = jit_hooks.stats_get_counter_value(None, jit.Counters.TOTAL_COMPILED_BRIDGES)
        vm_w_params[63] = interp.space.wrap_int(numberOfLoops + numberOfBridges)

    vm_w_params[69] = interp.space.wrap_int(constants.INTERP_PROXY_MAJOR)
    vm_w_params[70] = interp.space.wrap_int(constants.INTERP_PROXY_MINOR)

    if argcount == 0:
        return interp.space.wrap_list(vm_w_params)

    arg2_w = s_frame.pop()  # index (really the receiver, index has been removed above)
    if not isinstance(arg1_w, W_SmallInteger):
        raise PrimitiveFailedError
    if argcount == 1:
        if not 0 <= arg1_w.value <= 70:
            raise PrimitiveFailedError
        return vm_w_params[arg1_w.value - 1]

    s_frame.pop()  # new value
    if argcount == 2:
        # return the 'old value'
        return interp.space.wrap_int(0)

# list the n-th loaded module
@expose_primitive(VM_LOADED_MODULES, unwrap_spec=[object, int])
def func(interp, s_frame, w_rcvr, index):
    from rsqueakvm.primitives.control import ExternalPluginNames
    try:
        return interp.space.wrap_string(ExternalPluginNames[index])
    except IndexError:
        pass
    return interp.space.w_nil
