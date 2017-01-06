import re

API_PORT = 8082
QUEUE_PORT = 8083
CONTROL_PORT = 8084
BENCHMARK_MACHINES = [
    "http://fb12ce8ws06:%s" % QUEUE_PORT,
    "http://timfelgentreff.no-ip.org:%s" % QUEUE_PORT
]
JOB_TABLE = "jobs"
COMMITID = "commitid"
BRANCH = "branch"
VM = "vm"
FLAG = "done"
BENCHMARK_FIELD = "benchmark"
DBFILE = "benchmarks.db"
CODESPEED_URL = 'http://speed.squeak.org/'
BINARY_URL = "https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/{}"
BINARY_BASENAME = "rsqueak-x86-linux-jit-{}"

ITERATIONS = 100

OUTPUT_RE = re.compile(r"([a-zA-Z0-9]+) time: iterations=[0-9]+ runtime: ([0-9\.]+)ms \+/\-([0-9\.]+)")

BENCHMARKS = []

IMAGES = {
    "rsqueak": "Spur32.image",
    "cog": "Spur32.image",
    "interpreter": "V332.image",
    "rsqueak64": "Spur32.image",
    "squeakjs": "Spur32.image",
    "cog64": "Spur64.image",
    "stack": "Spur32.image",
    "stack64": "Spur64.image",
    "sista": "Scorch.image", # VM with counters and Sista is enabled
    "coldsista": "Scorch.image", # VM with counters and Sista is enabled, but flushed between iterations
    "counters": "Scorch.image", # VM with counters, but Sista is disabled
    "nocounters": "Scorch.image", # VM without counters
}

EXTRACODE = {
    "sista": "Smalltalk specialObjectsArray at: 59 put: #conditionalBranchCounterTrippedOn:.",
    "coldsista": """
        Smalltalk at: #RunCount put: 0.
        Smalltalk specialObjectsArray at: 59 put: #conditionalBranchCounterTrippedOn:.
        BenchmarkSuite recordResultsHook: [
            Smalltalk specialObjectsArray at: 59 put: nil.
            ((Smalltalk at: #RunCount) \\\\ 4) == 0 ifTrue: [
              CompiledMethod allInstances do: [:each |
                (each isOptimized and: [
                      each metadata isCustomized ifNil: [false]])
                  ifTrue: [SoDependencyMap default uninstallMethod: each]].
              FileStream stdout nextPutAll: 'recompiling ... '.
              OpalCompiler recompileAll.
              FileStream stdout nextPutAll: 'done'; cr.
              SoDependencyMap default debugFlushAll].
            Smalltalk at: #RunCount put: (Smalltalk at: #RunCount) + 1.
            Smalltalk specialObjectsArray at: 59 put: #conditionalBranchCounterTrippedOn:.
        ].
    """,
}

from benchmarks import BENCHMARKS
from config import *
