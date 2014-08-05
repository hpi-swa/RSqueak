import sys, os, shutil
from spyvm.util import logparser

def main(argv):
    if len(argv) != 1:
        print "Need pypy log-file as parameter."
        return 1
    logfile = argv[0]
    
    tracedir = logfile + "_traces"
    traces = logparser.extract_traces(logfile, remove_main_labels=False)
    print_traces(traces, tracedir)
    traces = logparser.extract_traces(logfile, remove_debug=False, remove_main_labels=False)
    print_traces(traces, os.path.join(tracedir, "debug"))
    
def print_traces(traces, tracedir):
    if os.path.exists(tracedir):
        shutil.rmtree(tracedir)
    os.mkdir(tracedir)
    
    for i, trace in enumerate(traces):
        basename = os.path.join(tracedir, "loop_" + str(i))
        print_trace_part(trace.loop, basename + '_main')
        print_trace_part(trace.setup, basename + '_setup')
        for bridge_num, bridge in enumerate(trace.bridges):
            print_trace_part(bridge, basename + "_bridge_" + str(bridge_num))
    
def print_trace_part(trace, filename):
    if trace:
        file = open(filename, 'w')
        for t in trace:
            file.write(str(t))
            file.write('\n')

if __name__ == "__main__":
   main(sys.argv[1:])
