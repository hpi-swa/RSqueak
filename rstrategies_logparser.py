
import re, os, sys, operator

# TODO generalize this - read all operations occuring in the logfile
OPERATIONS = ["Filledin", "Initialized", "Switched"]

IMAGE_LOADING_STORAGE = " Image Loading Storage" # Space to be sorted to the beginning
OBJECT_CREATION_STORAGE = " Object Creation Storage"

# ====================================================================
# ======== Logfile parsing
# ====================================================================

def parse(filename, flags, callback):
    parsed_entries = 0
    if filename == "-":
        opener = lambda: sys.stdin
    else:
        opener = lambda: open(filename, 'r', 1)
    with opener() as file:
        while True:
            line = file.readline()
            if len(line) == 0:
                break
            entry = parse_line(line, flags)
            if entry:
                parsed_entries += 1
                callback(entry)
    return parsed_entries

line_pattern = re.compile("^(?P<operation>\w+) \(((?P<old>\w+) -> )?(?P<new>\w+)\)( of (?P<classname>.+))? size (?P<size>[0-9]+)( objects (?P<objects>[0-9]+))?( elements: (?P<classnames>.+( .+)*))?$")

def parse_line(line, flags):
    result = line_pattern.match(line)
    if result is None:
        if flags.verbose:
            print "Could not parse line: %s" % line[:-1]
        return None
    operation = result.group('operation')
    old_storage = result.group('old')
    new_storage = result.group('new')
    classname = result.group('classname')
    size = result.group('size')
    objects = result.group('objects')
    classnames = result.group('classnames')
    if classnames is not None:
        classnames = classnames.split(' ')
    
    return LogEntry(operation, old_storage, new_storage, classname, size, objects, classnames)

class LogEntry(object):
    
    def __init__(self, operation, old_storage, new_storage, classname, size, objects, classnames):
        self.operation = str(operation)
        self.new_storage = str(new_storage)
        self.classname = str(classname)
        self.size = int(size)
        self.objects = int(objects) if objects else 1
        self.classnames = set(classnames) if classnames else set()
        
        if old_storage is None:
            if operation == "Filledin":
                old_storage = IMAGE_LOADING_STORAGE
            elif operation == "Initialized":
                old_storage = OBJECT_CREATION_STORAGE
            else:
                assert False, "old_storage has to be available in a Switched operation"
        self.old_storage = str(old_storage)
        assert old_storage != new_storage, "old and new storage identical in log entry: %s" % self
    
    def clear_old_storage(self):
        if self.old_storage in (IMAGE_LOADING_STORAGE, OBJECT_CREATION_STORAGE):
            self.old_storage = None
    
    def full_key(self):
        return (self.operation, self.old_storage, self.new_storage)
    
    def __lt__(self, other):
        return self.classname < other.classname
    
    def __repr__(self):
        return "%s(%s)" % (self.__str__(), object.__repr__(self))
    
    def __str__(self):
        old_storage_string = "%s -> " % self.old_storage if self.old_storage else ""
        classname_string = " of %s" % self.classname if self.classname else ""
        objects_string = " objects %d" % self.objects if self.objects > 1 else ""
        return "%s (%s%s)%s size %d%s" % (self.operation, old_storage_string, self.new_storage, classname_string, self.size, objects_string)

# ====================================================================
# ======== Graph parsing
# ====================================================================

class Operations(object):
    
    def __init__(self, objects=0, slots=0, element_classnames=[]):
        self.objects = objects
        self.slots = slots
        self.element_classnames = set(element_classnames)
    
    def __str__(self, total=None):
        if self.objects == 0:
            avg_slots = 0
        else:
            avg_slots = float(self.slots) / self.objects
        if total is not None and total.slots != 0:
            percent_slots = " (%.1f%%)" % (float(self.slots)*100 / total.slots)
        else:
            percent_slots = ""
        if total is not None and total.objects != 0:
            percent_objects = " (%.1f%%)" % (float(self.objects)*100 / total.objects)
        else:
            percent_objects = ""
        slots = format(self.slots, ",d")
        objects = format(self.objects, ",d")
        classnames = (" [ elements: %s ]" % ' '.join([str(x) for x in self.element_classnames])) \
                                    if len(self.element_classnames) else ""
        return "%s%s slots in %s%s objects (avg size: %.1f)%s" % (slots, percent_slots, objects, percent_objects, avg_slots, classnames)
    
    def __repr__(self):
        return "%s(%s)" % (self.__str__(), object.__repr__(self))
    
    def add_log_entry(self, entry):
        self.slots = self.slots + entry.size
        self.objects = self.objects + entry.objects
        self.element_classnames |= entry.classnames
    
    def __sub__(self, other):
        return Operations(self.objects - other.objects, self.slots - other.slots)
    
    def __add__(self, other):
        return Operations(self.objects + other.objects, self.slots + other.slots)
    
    def __lt__(self, other):
        return self.slots < other.slots
    
    def empty(self):
        return self.objects == 0 and self.slots == 0
    
    def prefixprint(self, key="", total=None):
        if not self.empty():
            print "%s%s" % (key, self.__str__(total))
    
class ClassOperations(object):
    
    def __init__(self):
        self.classes = {}
    
    def cls(self, name):
        if name not in self.classes:
            self.classes[name] = Operations()
        return self.classes[name]
    
    def total(self):
        return reduce(operator.add, self.classes.values(), Operations())
    
    def __str__(self):
        return "ClassOperations(%s)" % self.classes
    
    def __repr__(self):
        return "%s(%s)" % (self.__str__(), object.__repr__(self))
    
    def __add__(self, other):
        result = ClassOperations()
        result.classes = dict(self.classes)
        for classname, other_class in other.classes.items():
            result.cls(classname) # Make sure exists.
            result.classes[classname] += other_class
        return result
    
    def __sub__(self, other):
        result = ClassOperations()
        result.classes = dict(self.classes)
        for classname, other_class in other.classes.items():
            result.cls(classname) # Make sure exists.
            result.classes[classname] -= other_class
        return result
    
class StorageEdge(object):
    
    def __init__(self, operation="None", origin=None, target=None):
        assert operation == "None" or operation in OPERATIONS, "Unknown operation %s" % operation
        self.operation = operation
        self.classes = ClassOperations()
        self.origin = origin
        self.target = target
        
        if self.origin is not None and self.origin is self.target and self.operation != "Filledin":
            import pdb; pdb.set_trace()
    
    def full_key(self):
        return (self.operation, self.origin.name, self.target.name)
    
    def cls(self, classname):
        return self.classes.cls(classname)
    
    def total(self):
        return self.classes.total()
    
    def notify_nodes(self):
        self.origin.note_outgoing(self)
        self.target.note_incoming(self)
    
    def add_log_entry(self, entry):
        self.cls(entry.classname).add_log_entry(entry)
    
    def as_log_entries(self):
        entries = []
        for classname, ops in self.classes.classes.items():
            entry = LogEntry(self.operation, self.origin.name, self.target.name, classname, ops.slots, ops.objects, ops.element_classnames)
            entry.clear_old_storage()
            entries.append(entry)
        return entries
    
    def __lt__(self, other):
        return self.full_key() < other.full_key()
    
    def __str__(self):
        return "[%s %s -> %s]" % (self.operation, self.origin, self.target)
    
    def __repr__(self):
        return "%s(%s)" % (self.__str__(), object.__repr__(self))
    
    def __add__(self, other):
        origin = self.origin if self.origin is not None else other.origin
        target = self.target if self.target is not None else other.target
        result = StorageEdge(self.operation, origin, target)
        result.classes += self.classes + other.classes
        return result
    
    def __sub__(self, other):
        origin = self.origin if self.origin is not None else other.origin
        target = self.target if self.target is not None else other.target
        result = StorageEdge(self.operation, origin, target)
        result.classes += self.classes - other.classes
        return result
    
class StorageNode(object):
    
    def __init__(self, name):
        self.name = name
        self.incoming = set()
        self.outgoing = set()
    
    def note_incoming(self, edge):
        assert edge.target is self
        if edge not in self.incoming:
            self.incoming.add(edge)
        
    def note_outgoing(self, edge):
        assert edge.origin is self
        if edge not in self.outgoing:
            self.outgoing.add(edge)
        
    def incoming_edges(self, operation):
        return filter(lambda x: x.operation == operation, self.incoming)
    
    def outgoing_edges(self, operation):
        return filter(lambda x: x.operation == operation, self.outgoing)
    
    def sum_incoming(self, operation):
        return reduce(operator.add, self.incoming_edges(operation), StorageEdge(operation))
        
    def sum_outgoing(self, operation):
        return reduce(operator.add, self.outgoing_edges(operation), StorageEdge(operation))
    
    def sum_all_incoming(self):
        return reduce(operator.add, self.incoming, StorageEdge())
    
    def sum_all_outgoing(self):
        return reduce(operator.add, self.outgoing, StorageEdge())
    
    def __str__(self):
        return self.name
    
    def __repr__(self):
        return "%s(%s)" % (self.__str__(), object.__repr__(self))
    
    def merge_edge_sets(self, set1, set2, key_slot):
        getter = lambda edge: edge.__dict__[key_slot]
        set_dict = dict([(getter(edge), edge) for edge in set1])
        for edge in set2:
            key = getter(edge)
            if key not in set_dict:
                set_dict[key] = edge
            else:
                set_dict[key] += edge
        return set(set_dict.values())
    
    def __add__(self, other):
        result = StorageNode("%s %s" % (self.name, other.name))
        result.incoming = self.merge_edge_sets(self.incoming, other.incoming, "origin")
        # TODO bullshit code
        for edge in result.incoming:
            edge.target = result
        result.outgoing = self.merge_edge_sets(self.outgoing, other.outgoing, "target")
        for edge in result.outgoing:
            edge.origin = result
        return result
    
    def __lt__(self, other):
        return self.name < other.name
    
class StorageGraph(object):
    
    def __init__(self):
        self.nodes = {}
        self.edges = {}
    
    def node(self, name):
        if name not in self.nodes:
            self.nodes[name] = StorageNode(name)
        return self.nodes[name]
    
    def assert_sanity(self):
        visited_edges = set()
        for node in self.nodes.values():
            for edge in node.incoming:
                assert edge in self.edges.values(), "Edge not in graph's edges: %s" % edge
                visited_edges.add(edge)
                if not edge.target is node:
                    print "Wrong edge target: %s\nIncoming edge: %s\nIn node: %s" % (edge.target, edge, node)
                    assert False
                if not edge in edge.origin.outgoing:
                    print "Edge not in origin's outgoing: %s\nIncoming edge: %s\nIn node: %s" % (edge.origin.outgoing, edge, node)
                    assert False
            for edge in node.outgoing:
                assert edge in self.edges.values(), "Edge not in graph's edges: %s" % edge
                visited_edges.add(edge)
                if not edge.origin is node:
                    print "Wrong edge origin: %s\nOutgoing edge: %s\nIn node: %s" % (edge.origin, edge, node)
                    assert False
                if not edge in edge.target.incoming:
                    print "Edge not in origin's incoming: %s\nOutgoing edge: %s\nIn node: %s" % (edge.target.incoming, edge, node)
                    assert False
        assert len(visited_edges) == len(self.edges.values()), "Not all of graph's edges visited."
    
    def add_log_entry(self, log_entry):
        key = log_entry.full_key()
        if key not in self.edges:
            edge = StorageEdge(log_entry.operation, self.node(log_entry.old_storage), self.node(log_entry.new_storage))
            self.edges[key] = edge
            edge.notify_nodes()
        self.edges[key].add_log_entry(log_entry)
    
    def collapse_nodes(self, collapsed_nodes, new_name=None):
        if len(collapsed_nodes) == 0:
            return
        for node in collapsed_nodes:
            del self.nodes[node.name]
            for edge in node.incoming:
                del self.edges[edge.full_key()]
            for edge in node.outgoing:
                del self.edges[edge.full_key()]
        new_node = reduce(operator.add, collapsed_nodes)
        if new_name is not None:
            new_node.name = new_name
        self.nodes[new_node.name] = new_node
        # TODO bullshit code
        for node in collapsed_nodes:
            for edge in node.incoming:
                edge.origin.outgoing.remove(edge)
                new_edges = filter(lambda filtered: filtered.origin == edge.origin, new_node.incoming)
                assert len(new_edges) == 1
                edge.origin.outgoing.add(new_edges[0])
            for edge in node.outgoing:
                edge.target.incoming.remove(edge)
                new_edges = filter(lambda filtered: filtered.target == edge.target, new_node.outgoing)
                assert len(new_edges) == 1
                edge.target.incoming.add(new_edges[0])
        for edge in new_node.incoming:
            self.edges[edge.full_key()] = edge
        for edge in new_node.outgoing:
            self.edges[edge.full_key()] = edge
        self.assert_sanity()
    
    def split_nodes(self, new_name=None):
        nodes = filter(lambda node: "Storage" not in node.name, self.nodes.values())
        self.collapse_nodes(nodes, new_name)
    
    def sorted_nodes(self):
        nodes = self.nodes.values()
        nodes.sort()
        return nodes
    
def make_graph(logfile, flags):
    graph = StorageGraph()
    def callback(entry):
        graph.add_log_entry(entry)
    parse(logfile, flags, callback)
    graph.assert_sanity()
    return graph

# ====================================================================
# ======== Command - Summarize log content
# ====================================================================

def command_summarize(logfile, flags):
    graph = make_graph(logfile, flags)
    if not flags.allstorage:
        graph.split_nodes()
    for node in graph.sorted_nodes():
        node.print_summary(flags)

def StorageNode_print_summary(self, flags):
    print "\n%s:" % self.name
    sum = StorageEdge()
    total_incoming = self.sum_all_incoming().total() if flags.percent else None
    
    print "\tIncoming:"
    for operation in OPERATIONS:
        if flags.detailed:
            edges = [ (edge.origin.name, edge) for edge in self.incoming_edges(operation) ]
        else:
            edges = [ (operation, self.sum_incoming(operation)) ]
        for edgename, edge in edges:
            edge.print_with_name("\t\t\t", edgename, total_incoming, flags)
            sum += edge
    
    print "\tOutgoing:"
    for operation in OPERATIONS:
        if flags.detailed:
            edges = [ (edge.target.name, edge) for edge in self.outgoing_edges(operation) ]
        else:
            edges = [ (operation, self.sum_outgoing(operation)) ]
        for edgename, edge in edges:
            edge.print_with_name("\t\t\t", edgename, total_incoming, flags)
            sum -= edge
    
    sum.print_with_name("\t", "Remaining", total_incoming, flags)

StorageNode.print_summary = StorageNode_print_summary

def StorageEdge_print_with_name(self, prefix, edgename, total_reference, flags):
    if flags.classes:   
        print "%s%s:" % (prefix, edgename)
        prefix += "\t\t"
        operations = self.classes.classes.items()
        operations.sort(reverse=True, key=operator.itemgetter(1))
    else:
        operations = [ (edgename, self.total()) ]
    for classname, classops in operations:
        classops.prefixprint("%s%s: " % (prefix, classname), total_reference)
    
StorageEdge.print_with_name = StorageEdge_print_with_name

# ====================================================================
# ======== Command - DOT output
# ====================================================================

# Output is valid dot code and can be parsed by the graphviz dot utility.
def command_print_dot(logfile, flags):
    graph = make_graph(logfile, flags)
    print "/*"
    print "Storage Statistics (dot format):"
    print "================================"
    print "*/"
    print dot_string(graph, flags)

def command_dot(logfile, flags):
    import subprocess
    dot = dot_string(make_graph(logfile, flags), flags)
    command = ["dot", "-Tjpg", "-o%s.jpg" % flags.logfile]
    print "Running:\n%s" % " ".join(command)
    p = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    output = p.communicate(input=dot)[0]
    print output

def dot_string(graph, flags):
    result = "digraph G {"
    incoming_cache = {}
    if not flags.allstorage:
        graph.split_nodes("Other")
    
    for node in graph.nodes.values():
        incoming = node.sum_all_incoming().total()
        outgoing = node.sum_all_outgoing().total()
        remaining = incoming - outgoing
        if remaining.objects < 0:
            # TODO This is a special node. Hacky way to find out.
            incoming_cache[node.name] = outgoing
            shape = ",shape=box"
            label = "\nObjects: %s" % format(outgoing.objects, ",.0f")
            label += "\nSlots: %s" % format(outgoing.slots, ",.0f")
        else:
            incoming_cache[node.name] = incoming
            shape = ""
            label = "\nIncoming objects: %s" % format(incoming.objects, ",.0f")
            label += "\nIncoming slots: %s" % format(incoming.slots, ",.0f")
            if remaining.objects != incoming.objects:
                if flags.percent and incoming.objects != 0:
                    percent_remaining_objects = " (%.1f%%)" % (float(remaining.objects)*100 / incoming.objects)
                    percent_remaining_slots = " (%.1f%%)" % (float(remaining.slots)*100 / incoming.slots)
                else:
                    percent_remaining_objects = percent_remaining_slots = ""
                label += "\nRemaining objects: %s%s" % (format(remaining.objects, ",.0f"), percent_remaining_objects)
                label += "\nRemaining slots: %s%s" % (format(remaining.slots, ",.0f"), percent_remaining_slots)
        result += "%s [label=\"%s%s\"%s];" % (node.name.replace(" ", "_"), node.name, label, shape)
    
    for edge in graph.edges.values():
        total = edge.total()
        str_objects = "%s objects" % format(total.objects, ",.0f")
        str_slots = "%s slots" % format(total.slots, ",.0f")
        incoming = incoming_cache[edge.origin.name]
        if flags.percent and incoming.objects != 0:
            str_objects += " (%.1f%%)" % (float(total.objects)*100 / incoming.objects)
            str_slots += " (%.1f%%)" % (float(total.slots)*100 / incoming.slots)
        
        target_node = edge.target.name.replace(" ", "_")
        source_node = edge.origin.name.replace(" ", "_")
        result += "%s -> %s [label=\"%s\n%s\n%.1f slots per object\"];" % (source_node, target_node, str_objects, str_slots, total.slots / total.objects)
    
    result += "}"
    return result

# ====================================================================
# ======== Other commands
# ====================================================================

def command_aggregate(logfile, flags):
    graph = make_graph(logfile, flags)
    edges = graph.edges.values()
    edges.sort()
    for edge in edges:
        logentries = edge.as_log_entries()
        logentries.sort()
        for entry in logentries:
            print entry

def command_print_entries(logfile, flags):
    def callback(entry):
        print entry
    parse(logfile, flags, callback)

# ====================================================================
# ======== Main
# ====================================================================

class Flags(object):
    
    def __init__(self, flags):
        self.flags = {}
        for name, short in flags:
            self.__dict__[name] = False
            self.flags[short] = name
    
    def handle(self, arg):
        if arg in self.flags:
            self.__dict__[self.flags[arg]] = True
            return True
        else:
            return False
    
    def __str__(self):
        descriptions = [ ("%s (%s)" % description) for description in self.flags.items() ]
        return "[%s]" % " | ".join(descriptions)
    
def usage(flags, commands):
    print "Arguments: logfile command %s" % flags
    print "Available commands: %s" % commands
    exit(1)

def main(argv):
    flags = Flags([
        ('verbose', '-v'),
        ('percent', '-p'),
        ('allstorage', '-a'),
        ('detailed', '-d'),
        ('classes', '-c'),
    ])
    
    command_prefix = "command_"
    module = sys.modules[__name__].__dict__
    commands = [ a[len(command_prefix):] for a in module.keys() if a.startswith(command_prefix) ]
    
    if len(argv) < 2:
        usage(flags, commands)
    logfile = argv[0]
    flags.logfile = logfile
    command = argv[1]
    for flag in argv[2:]:
        if not flags.handle(flag):
            usage(flags, commands)
    if command not in commands:
        usage(flags, commands)
    
    func = module[command_prefix + command]
    func(logfile, flags)

if __name__ == "__main__":
    main(sys.argv[1:])
