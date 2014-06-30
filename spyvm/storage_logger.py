
# Put flag in a list to make it modifyable after compile time.
_active = [False]

def activate():
    _active[0] = True

def log(w_obj, operation, old_storage_object=None, log_classname=True):
    if not _active[0]:
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
    
    # Construct and print the logstring
    old_storage_string = "%s -> " % old_storage if old_storage else ""
    classname_string = " of %s" % classname if classname else ""
    print "%s (%s%s)%s size %d" % (operation, old_storage_string, new_storage, classname_string, size)
