
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
    modules = []
    using_classname = False
    
    def add_module(self, module):
        if module not in self.modules:
            self.modules.append(module)
            self.using_classname = self.using_classname or module.uses_classname
    
    def log(self, w_obj, operation, old_storage_object, log_classname):
        if len(self.modules) > 0:
            new_storage = w_obj.shadow.repr_classname
            if old_storage_object:
                old_storage = old_storage_object.repr_classname
            else:
                old_storage = None
            size = w_obj.size()
            
            key = self.make_key(operation, old_storage, new_storage)
            if self.using_classname and log_classname:
                classname = w_obj.guess_classname()
            else:
                classname = None
            for module in self.modules:
                module.storage_operation(key, size, classname)
    
    def make_key(self, operation, old_storage, new_storage):
        return (operation, old_storage, new_storage)
        
    def print_results(self):
        for module in self.modules:
            module.print_results()

class StatisticsModule(object):
    uses_classname = False
    def storage_operation(self, operation_key, storage_size, element_classname):
        raise NotImplementedError("Abstract class")
    def print_results(self):
        raise NotImplementedError("Abstract class")
    def key_string(self, key):
        if key[1]:
            return "%s (%s -> %s)" % (key[0], key[1], key[2])
        else:
            return "%s (%s)" % (key[0], key[2])

class StatisticsLogger(StatisticsModule):
    uses_classname = True
    def storage_operation(self, operation_key, storage_size, element_classname):
        print self.log_string(operation_key, storage_size, element_classname)
    
    def log_string(self, operation_key, storage_size, element_classname):
        if element_classname:
            return "%s of %s size %d" % (self.key_string(operation_key), element_classname, storage_size)
        else:
            return "%s size %d" % (self.key_string(operation_key), storage_size)
    
    def print_results(self):
        # Nothing to do, this is just for logging during runtime.
        pass

class AbstractStatisticsCollector(StatisticsModule):
    stats = {}
    
    def storage_operation(self, operation_key, storage_size, element_classname):
        if not operation_key in self.stats:
            self.stats[operation_key] = self.initial_value()
        self.increment_value(self.stats[operation_key], storage_size)
    
    def sorted_keys(self):
        keys = [ x for x in self.stats ]
        StatsSorter(keys).sort()
        return keys

class StatisticsCollector(AbstractStatisticsCollector):
    # Value: [total_size, num_operations]
    def initial_value(self): return [0, 0]
    def increment_value(self, value_object, storage_size):
        value_object[0] = value_object[0] + storage_size
        value_object[1] = value_object[1] + 1
    def print_results(self):
        print "Storage Statistics:"
        for key in self.sorted_keys():
            tuple = self.stats[key]
            sum = tuple[0]
            num = tuple[1]
            print "\t%s: %d times, avg size: %f" % (self.key_string(key), num, float(sum)/num)

class DetailedStatisticsCollector(AbstractStatisticsCollector):
    # Value: list of numbers (sizes)
    def initial_value(self): return []
    def increment_value(self, value_object, storage_size):
        value_object.append(storage_size)
    def print_results(self):
        print "Detailed Storage Statistics:"
        for key in self.sorted_keys():
            print "\t%s: s" % (self.key_string(key), self.stats[key])

# Static & global access to a StorageStatistics instance.

_stats = StorageStatistics()
_logger = StatisticsLogger()
_collector = StatisticsCollector()
_detailedcollector = DetailedStatisticsCollector()

def activate_statistics(log=False, statistics=False, detailed_statistics=False):
    if log:
        _stats.add_module(_logger)
    if statistics:
        _stats.add_module(_collector)
    if detailed_statistics:
        _stats.add_module(_detailedcollector)

def print_statistics():
    _stats.print_results()

def log(w_obj, operation, old_storage=None, log_classname=True):
    _stats.log(w_obj, operation, old_storage, log_classname)
