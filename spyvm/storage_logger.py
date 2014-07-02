
class Logger(object):
    def __init__(self):
        self.active = False
        self.aggregate = False
        self.logs = {}
    
    def log(self, operation, old_storage, new_storage, classname, size):
        if self.aggregate:
            key = (operation, old_storage, new_storage, classname)
            if key not in self.logs:
                self.logs[key] = [0, 0]
            tuple = self.logs[key]
            tuple[0] += size
            tuple[1] += 1
        else:
            self.output(operation, old_storage, new_storage, classname, size, 1)
    
    def print_aggregated_log(self):
        if not self.aggregate:
            return
        for key, tuple in self.logs.items():
            operation, old_storage, new_storage, classname = key
            slots, objects = tuple
            self.output(operation, old_storage, new_storage, classname, slots, objects)
    
    def output(self, operation, old_storage, new_storage, classname, slots, objects):
        old_storage_string = "%s -> " % old_storage if old_storage else ""
        classname_string = " of %s" % classname if classname else ""
        format = (operation, old_storage_string, new_storage, classname_string, slots, objects)
        print "%s (%s%s)%s size %d objects %d" % format

_logger = Logger()

def activate(aggregate=False):
    _logger.active = True
    _logger.aggregate = aggregate

def print_aggregated_log():
    _logger.print_aggregated_log()

def log(w_obj, operation, old_storage_object=None, log_classname=True):
    if not _logger.active:
        return
    
    # Gather information to be logged
    new_storage = w_obj.shadow.repr_classname
    if old_storage_object:
        old_storage = old_storage_object.repr_classname
    else:
        old_storage = None
    size = w_obj.size()
    if log_classname:
        classname = w_obj.guess_classname()
    else:
        classname = None
    
    _logger.log(operation, old_storage, new_storage, classname, size)
    