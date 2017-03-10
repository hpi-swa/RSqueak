import os
import sys

from rsqueakvm.util import system


class PluginRegistry(object):

    def __init__(self):
        self._enabled_names = []
        self._enabled_plugins = []
        self._disabled_names = []

    def add(self, instance):
        name = instance.name()
        try:
            # replace instance of an enabled plugin after reload
            index = self._enabled_names.index(name)
            self._enabled_plugins[index] = instance
        except ValueError:
            if instance.is_enabled():
                self._enabled_names.append(name)
                self._enabled_plugins.append(instance)
            else:
                if name not in self._disabled_names:
                    self._disabled_names.append(name)

    def enabled_names(self):
        return self._enabled_names

    def enabled_plugins(self):
        return self._enabled_plugins

    def disabled_names(self):
        return self._disabled_names


registry = PluginRegistry()


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
            else:
                reload(sys.modules.get(module_string))
        except ImportError as e:
            print e
            import pdb; pdb.set_trace()

    disabled_names += registry.disabled_names()

    # remove data which is not needed during translation
    if '--shell' not in sys.argv:
        del registry._disabled_names

    print 'Building with\n\t' + '\n\t'.join(registry.enabled_names())
    print 'Disabled plugins\n\t' + '\n\t'.join(disabled_names)
    return registry.enabled_names(), registry.enabled_plugins()
