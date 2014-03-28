
from rpython.rlib.listsort import TimSort

class StatsSorter(TimSort):
    """Sort a tuple of 3 strings"""
    def lt(self, a, b):
        if a[0] == b[0]:
            if a[1] == b[1]:
                return a[2] < b[2]
            else:
                return a[1] < b[1]
        else:
            return a[0] < b[0]

class StorageStatistics(object):
    # Key: (operation_name, old_storage, new_storage)
    # Value: [sizes]
    stats = {}
    
    do_log = False
    do_stats = False
    do_stats_sizes = False
    
    def log(self, w_obj, operation, old_storage_object, log_classname):
        if self.do_log or self.do_stats:
            new_storage = w_obj.shadow.repr_classname
            if old_storage_object:
                old_storage = old_storage_object.repr_classname
            else:
                old_storage = None
            size = w_obj.size()
            
            key = self.make_key(operation, old_storage, new_storage)
            if _stats.do_stats:
                self.stat_operation(key, size)
            if self.do_log:
                if log_classname:
                    classname = w_obj.guess_classname()
                else:
                    classname = None
                self.log_operation(key, size, classname)
    
    def make_key(self, operation, old_storage, new_storage):
        return (operation, old_storage, new_storage)
    
    def stat_operation(self, key, size):
        if not key in self.stats:
            self.stats[key] = []
        self.stats[key].append(size)

    def log_operation(self, key, size, classname):
        print self.log_operation_string(key, size, classname)
        
    def key_string(self, key):
        if key[1]:
            return "%s (%s -> %s)" % (key[0], key[1], key[2])
        else:
            return "%s (%s)" % (key[0], key[2])
        
    def log_operation_string(self, key, size, classname):
        if classname:
            return "%s of %s size %d" % (self.key_string(key), classname, size)
        else:
            return "%s size %d" % (self.key_string(key), size)
        
    def sorted_keys(self):
        keys = [ x for x in self.stats ]
        StatsSorter(keys).sort()
        return keys
        
    def print_stats(self):
        for key in self.sorted_keys():
            sizes = self.stats[key]
            sum = 0
            for s in sizes: sum += s
            print "%s: %d times, avg size: %f" % (self.key_string(key), len(sizes), sum/len(sizes))
            if self.do_stats_sizes:
                print "       All sizes: %s" % sizes

_stats = StorageStatistics()

def activate_statistics(log=False, statistics=False, statstics_sizes=False):
    _stats.do_log = _stats.do_log or log
    _stats.do_stats = _stats.do_stats or statistics
    _stats.do_stats_sizes = _stats.do_stats_sizes or statstics_sizes

def print_statistics():
    if _stats.do_stats:
        _stats.print_stats()

def log(w_obj, operation, old_storage=None, log_classname=True):
    _stats.log(w_obj, operation, old_storage, log_classname)
