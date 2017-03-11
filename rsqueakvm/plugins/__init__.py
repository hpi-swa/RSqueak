import os
import sys

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

    files = os.listdir(os.path.join(
        os.path.dirname(__file__), '..', 'plugins'))
    disabled_names = []

    # special...
    if 'JitHooks' not in system.optional_plugins:
        disabled_names.append('JitHooks')

    for filename in files:
        if filename.startswith('_') or not filename.endswith('.py'):
            continue
        modulename = filename.replace('.py', '')
        try:
            module_string = 'rsqueakvm.plugins.%s' % modulename
            if module_string not in sys.modules:
                __import__(module_string)
            elif system.IS_SHELL:
                reload(sys.modules.get(module_string))
        except ImportError as e:
            print e
            import pdb; pdb.set_trace()

    disabled_names += PluginRegistry.disabled_names

    if not system.IS_SHELL:
        # remove data which is not needed during translation
        del PluginRegistry.disabled_names

    print 'Building with\n\t' + '\n\t'.join(PluginRegistry.enabled_names)
    print 'Disabled plugins\n\t' + '\n\t'.join(disabled_names)
    return PluginRegistry.enabled_names, PluginRegistry.enabled_plugins
