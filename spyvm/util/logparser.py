from rpython.tool import logparser
from rpython.tool.jitlogparser.parser import SimpleParser, Op
from rpython.tool.jitlogparser.storage import LoopStorage
from rpython.jit.tool import oparser
   
def extract_traces(file, remove_debug=True, remove_main_labels=True, remove_all_labels=False):
    data = logparser.parse_log_file(file, verbose=False)
    data = logparser.extract_category(data, "jit-log-opt-")
    
    storage = LoopStorage()
    traces = [SimpleParser.parse_from_input(t) for t in data]
    main_loops = storage.reconnect_loops(traces)
    traces_w = []
    for trace in traces:
        if trace in main_loops:
            traces_w.append(Trace(trace))
        else:
            traces_w[len(traces_w) - 1].addbridge(trace)
    for trace in traces_w:
        trace.parse(remove_debug, remove_main_labels, remove_all_labels)
    return traces_w

class Trace(object):
    def __init__(self, trace,):
        self._trace = trace
        self._bridgetraces = []
        self.bridges = None
        self.setup = None
        self.loop = None
    
    def addbridge(self, trace):
        self._bridgetraces.append(trace)
    
    def parse(self, remove_debug, remove_main_labels, remove_all_labels):
        self.remove_debug, self.remove_main_labels, self.remove_all_labels = \
                            remove_debug, remove_main_labels, remove_all_labels
        self.parse_loop()
        self.parse_bridges()
    
    def keep_op(self, op):
        return \
            (not self.remove_debug or not op.name.startswith("debug_")) and \
            (not self.remove_all_labels or not op.name == "label")
    
    def parse_bridges(self):
        self.bridges = []
        for bridge in self._bridgetraces:
            self.bridges.append([op for op in bridge.operations if self.keep_op(op)])
    
    def parse_loop(self):
        self.loop = []
        self.setup = []
        label_seen = None
        for idx, op in enumerate(self._trace.operations):
            if op.name == "label":
                if label_seen is None: # first label
                    label_seen = False
                    if self.remove_main_labels: continue
                elif label_seen is False:
                    label_seen = True # second label
                    if self.remove_main_labels: continue
            if self.keep_op(op):
                if label_seen:
                    self.loop.append(op)
                else:
                    self.setup.append(op)
    