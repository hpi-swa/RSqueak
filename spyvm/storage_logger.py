
# Put flags in an object to make it modifyable after compile time.
class LoggerOptions(object):
    def __init__(self):
        self.active = False
        self.binary = False

_options = LoggerOptions()

def activate(binary = False):
    _options.active = True
    _options.binary = binary

def log(w_obj, operation, old_storage_object=None, log_classname=True):
    if not _options.active:
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
    
    if _options.binary:
        binary_output(operation, old_storage, new_storage, classname, size)
    else:
        output(operation, old_storage, new_storage, classname, size)

def output(operation, old_storage, new_storage, classname, size):
    # Construct and print a simple logstring
    old_storage_string = "%s -> " % old_storage if old_storage else ""
    classname_string = " of %s" % classname if classname else ""
    print "%s (%s%s)%s size %d" % (operation, old_storage_string, new_storage, classname_string, size)
    
operation_map = {
    "Filledin": 1,
    "Initialized": 2,
    "Switched": 3,
}

storage_map = {
    "AllNilStorageShadow": 1,
    "SmallIntegerOrNilStorageShadow": 2,
    "FloatOrNilStorageShadow": 3,
    "ListStorageShadow": 4,
    "WeakListStorageShadow": 5,
    "ClassShadow": 6,
    "MethodDictionaryShadow": 7,
    "BlockContextShadow": 8,
    "MethodContextShadow": 9,
    "CachedObjectShadow": 10,
    "ObserveeShadow": 11,
    None: 12,
}

def binary_output(operation, old_storage, new_storage, classname, size):
    # Output a byte-coded log entry
    bytes = [] # bytearray()
    
    # First 3 bytes: operation, old_storage, new_storage
    assert operation in operation_map, "Cannot handle operation %s" % operation
    bytes.append(operation_map[operation])
    assert old_storage in storage_map, "Cannot handle old-storage type %s" % old_storage
    bytes.append(storage_map[old_storage])
    assert new_storage in storage_map, "Cannot handle new-storage type %s" % new_storage
    bytes.append(storage_map[new_storage])
    
    # Next: 2 bytes encoding object size (big endian)
    assert size < 2**16, "Object of type %s too large (size %d)" % (classname, size)
    mask = (1<<8)-1
    bytes.append(size & mask)
    mask = mask<<8
    bytes.append((size & mask) >> 8)
    
    # Next: classname string plus terminating null-character
    i = 5
    if classname:
        for c in classname:
            bytes.append(ord(c))
            i += 1
    bytes.append(0)
    
    # No simpler way for RPython's sake.
    import os
    for b in bytes:
        os.write(1, chr(b))
