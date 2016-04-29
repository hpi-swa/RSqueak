import re

API_PORT = 8082
QUEUE_PORT = 8083
BENCHMARK_MACHINES = [
    "http://fb12ce8ws06:%s" % QUEUE_PORT
]
JOB_TABLE = "jobs"
COMMITID = "commitid"
BRANCH = "branch"
FLAG = "done"
DBFILE = "benchmarks.db"
CODESPEED_URL = 'http://172.16.64.134/'
BINARY_URL = "https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/{}"
BINARY_BASENAME = "rsqueak-x86-linux-jit-{}"

VMS = [
    "./cogspurlinux/squeak",
    "rsqueak"
]

BENCHMARKS = [
    "SMarkAStarBenchmark",
    "SMarkBinaryTreeBenchmark",
    "SMarkBlowfishBenchmark",
    "SMarkCompiler",
    "SMarkDeltaBlue",
    "SMarkFannkuch",
    "SMarkJSJson",
    "SMarkLoops",
    "SMarkMailinglistMicroBenchmarks",
    "SMarkMandelbrot",
    "SMarkMandelbrotBenchmark",
    "SMarkNBody",
    "SMarkPolymorphyBenchmark",
    "SMarkRSqueak",
    "SMarkRichards",
    "SMarkShootout",
    "SMarkSlopstone",
    "SMarkSmopstone",
    "SMarkSplayTreeBenchmark",
    "SMarkSqueakJS"
]

ITERATIONS = 100

OUTPUT_RE = re.compile(r"([a-zA-Z0-9]+) total: iterations=%s runtime: ([0-9\.]+)ms \+/\-([0-9\.]+)" % ITERATIONS)

from config import *
