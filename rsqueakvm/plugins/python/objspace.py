import os
import sys

from rsqueakvm.plugins.python.switching import SwitchToSmalltalkAction
from rsqueakvm.util import system

from pypy.module.sys.initpath import pypy_find_stdlib


def new_pypy_objspace():
    # This module is reloaded, but pypy_getudir has already been deleted
    from pypy.module import sys as pypy_sys
    reload(pypy_sys)
    # if 'pypy_getudir' not in Module.interpleveldefs:
    #     Module.interpleveldefs['pypy_getudir'] = 'foo'

    from pypy.config.pypyoption import get_pypy_config, set_pypy_opt_level
    translating = sys.argv[0] == '.build/build.py'  # make better
    pypy_config = get_pypy_config(translating=translating)

    # disable dispensable modules (to save compile time)
    pypy_config.objspace.usemodules.micronumpy = False
    pypy_config.objspace.usemodules.cppyy = False

    # cpyext causes a lot of 'Undefined symbols for architecture x86_64' errors
    pypy_config.objspace.usemodules.cpyext = False

    # disabling cffi backend for now, it also causes an undefined symbol error
    pypy_config.objspace.usemodules._cffi_backend = False

    # disable to save compile time
    from pypy.config.pypyoption import enable_allworkingmodules
    enable_allworkingmodules(pypy_config)

    from pypy.config.pypyoption import enable_translationmodules
    enable_translationmodules(pypy_config)

    # pypy_config.translation.check_str_without_nul = True

    # ensures pypy_hooks has a .space
    pypy_config.objspace.usemodules.pypyjit = True

    # rstacklets are required
    pypy_config.translation.continuation = True
    pypy_config.objspace.usemodules._continuation = True
    pypy_config.objspace.usemodules.thread = True

    # Enable immutable (and fast) module.Module
    pypy_config.objspace.std.suggest(withcelldict=True)

    # Enable more optimizations for PyPy's jit
    set_pypy_opt_level(pypy_config, 'jit')

    # Copy over some options that should be the same in both configs
    pypy_config.translation.make_jobs = system.translationconfig.make_jobs
    if system.translationconfig.output is not None:
        pypy_config.translation.output = system.translationconfig.output

    # merge_configs(config, pypy_config, 'RSqueak', 'PyPy')

    # PyPy needs threads
    pypy_config.translation.thread = True

    # Python objectspace ctor is not Rpython so create it here and
    # encapsulate it inside the entry point with a closure.
    from pypy.objspace.std import StdObjSpace as PyStdObjSpace

    py_space = PyStdObjSpace(pypy_config)

    # equivalent to the hack in app_main.py of PyPy, albiet interp-level.
    w_sys = py_space.sys
    w_modnames = w_sys.get('builtin_module_names')
    w_in = py_space.contains(w_modnames, py_space.newtext('__pypy__'))
    if not py_space.is_true(w_in):
        rl = py_space.sys.get('setrecursionlimit')
        py_space.call(rl, py_space.newlist([py_space.newint(5000)]))

    # Should always be able to import Python modules in CWD.
    w_sys_path = py_space.getattr(w_sys, py_space.newtext('path'))
    py_space.call_method(w_sys_path, 'append', py_space.newtext('.'))

    # Set sys.executable in PyPy -- some modules rely upon this existing.
    py_space.setattr(w_sys, py_space.newtext('executable'),
                     py_space.newtext(os.path.abspath(sys.argv[0])))

    # Set sys.(prefix|exec_prefix) in PyPy
    pypy_find_stdlib(py_space, sys.argv[0])

    return py_space


py_space = new_pypy_objspace()
switch_action = SwitchToSmalltalkAction(py_space)
py_space.actionflag.register_periodic_action(
    switch_action, use_bytecode_counter=True)
