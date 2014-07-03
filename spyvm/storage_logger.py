
class LogEntry(object):
    def __init__(self):
        self.slots = 0
        self.objects = 0
        self.element_classnames = {}
        
    def add(self, size, element_classname):
        self.slots += size
        self.objects += 1
        if element_classname:
            self.element_classnames[element_classname] = None
    
    def classnames(self):
        if len(self.element_classnames) > 0:
            return self.element_classnames.keys()
        return None

class Logger(object):
    def __init__(self):
        self.active = False
        self.aggregate = False
        self.elements = False
        self.logs = {}
    
    def log(self, operation, old_storage, new_storage, classname, size, element_classname):
        if self.aggregate:
            key = (operation, old_storage, new_storage, classname)
            if key not in self.logs:
                self.logs[key] = LogEntry()
            entry = self.logs[key]
            entry.add(size, element_classname)
        else:
            element_classnames = [ element_classname ] if element_classname else None
            self.output(operation, old_storage, new_storage, classname, size, 1, element_classnames)
    
    def print_aggregated_log(self):
        if not self.aggregate:
            return
        for key, entry in self.logs.items():
            operation, old_storage, new_storage, classname = key
            slots, objects, element_classnames = entry.slots, entry.objects, entry.classnames()
            self.output(operation, old_storage, new_storage, classname, slots, objects, element_classnames)
    
    def output(self, operation, old_storage, new_storage, classname, slots, objects, element_classnames):
        old_storage_string = "%s -> " % old_storage if old_storage else ""
        classname_string = " of %s" % classname if classname else ""
        element_string = (" elements: " + " ".join(element_classnames)) if element_classnames else ""
        format = (operation, old_storage_string, new_storage, classname_string, slots, objects, element_string)
        print "%s (%s%s)%s size %d objects %d%s" % format

_logger = Logger()

def activate(aggregate=False, elements=False):
    _logger.active = True
    _logger.aggregate = _logger.aggregate or aggregate
    _logger.elements = _logger.elements or elements

def print_aggregated_log():
    _logger.print_aggregated_log()

def log(w_obj, operation, old_storage_object=None, log_classname=True, w_element=None):
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
    if _logger.elements and w_element and log_classname:
        element_classname = w_element.guess_classname()
    else:
        element_classname = None
    
    _logger.log(operation, old_storage, new_storage, classname, size, element_classname)
    