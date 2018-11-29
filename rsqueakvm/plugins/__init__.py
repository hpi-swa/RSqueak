import sys
import py

from rsqueakvm.util import system


class PluginRegistry(object):
    enabled_names = []
    enabled_plugins = []
    disabled_names = []

    @staticmethod
    def add(instance):
        name = instance.name()
        try:
            # replace instance of an enabled plugin after reload
            index = PluginRegistry.enabled_names.index(name)
            PluginRegistry.enabled_plugins[index] = instance
        except ValueError:
            if instance.is_enabled():
                PluginRegistry.enabled_names.append(name)
                PluginRegistry.enabled_plugins.append(instance)
            else:
                if name not in PluginRegistry.disabled_names:
                    PluginRegistry.disabled_names.append(name)


def get_plugins():
    if system.without_plugins:
        print 'Plugins have been disabled'
        return [], []

    this_dir = py.path.local(__file__).dirpath()
    for filename in this_dir.listdir(should_load_plugin):
        modulename = filename.purebasename
        try:
            module_string = 'rsqueakvm.plugins.%s' % modulename
            if module_string not in sys.modules:
                __import__(module_string)
            elif system.IS_SHELL:
                reload(sys.modules.get(module_string))
        except ImportError as e:
            print e
            import pdb; pdb.set_trace()

    print_plugin_overview()
    if not system.IS_SHELL:
        # remove data which is not needed during translation
        del PluginRegistry.disabled_names
    return PluginRegistry.enabled_names, PluginRegistry.enabled_plugins

def should_load_plugin(localfile):
    if not localfile.fnmatch('[!_]*.py'): # "normal" py files only
        return False
    # The profiler plugin cannot be non-loaded safely once imported, hence
    # an early check
    if localfile.basename == 'profiler_plugin.py' and \
       'ProfilerPlugin' not in system.optional_plugins:
        return False
    return True

def print_plugin_overview():
    disabled_names = [name.strip() for name in system.disabled_plugins.split(',')]
    # special...
    if 'JitHooks' not in system.optional_plugins:
        disabled_names.append('JitHooks')
    disabled_names += PluginRegistry.disabled_names
    print 'Building with\n\t' + '\n\t'.join(PluginRegistry.enabled_names)
    print 'Disabled plugins\n\t' + '\n\t'.join(disabled_names)
