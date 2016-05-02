import re

API_PORT = 8082
QUEUE_PORT = 8083
CONTROL_PORT = 8084
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
    "rsqueak",
    "./cogspurlinux/squeak",
]

BENCHMARKS = [
    "SMarkAStarBenchmark.benchAStar",
    "SMarkBinaryTreeBenchmark.benchBinaryTree",
    "SMarkBlowfishBenchmark.benchBlowfish",
    "SMarkCompiler.benchCompiler",
    "SMarkDeltaBlue.benchDeltaBlue",
    "SMarkFannkuch.benchFannkuch",
    "SMarkGSGraphSearch.benchGraphSearch",
    "SMarkJSJson.benchJson",
    "SMarkLoops.benchArrayAccess",
    "SMarkLoops.benchClassVarBinding",
    "SMarkLoops.benchDoesNotUnderstand",
    "SMarkLoops.benchFloatLoop",
    "SMarkLoops.benchInstVarAccess",
    "SMarkLoops.benchIntLoop",
    "SMarkLoops.benchObjectAsMethod",
    "SMarkLoops.benchSend",
    "SMarkLoops.benchSendPrimitive",
    "SMarkLoops.benchSendWithManyArguments",
    "SMarkMailinglistMicroBenchmarks.benchByteStringHash",
    "SMarkMailinglistMicroBenchmarks.benchLRUCachePrintString",
    "SMarkMailinglistMicroBenchmarks.benchOrderedCollectionInsertFirst",
    "SMarkMailinglistMicroBenchmarks.benchOrderedCollectionRandomInsert",
    "SMarkMailinglistMicroBenchmarks.benchWideStringHash",
    "SMarkMandelbrot.benchMandelbrot",
    "SMarkMandelbrotBenchmark.benchMandelbrotIterative1Thread",
    "SMarkMandelbrotBenchmark.benchMandelbrotIterative2Thread",
    "SMarkMandelbrotBenchmark.benchMandelbrotIterative4Thread",
    "SMarkMandelbrotBenchmark.benchMandelbrotIterative8Thread",
    "SMarkMandelbrotBenchmark.benchMandelbrotRecursive1Thread",
    "SMarkMandelbrotBenchmark.benchMandelbrotRecursive2Thread",
    "SMarkMandelbrotBenchmark.benchMandelbrotRecursive4Thread",
    "SMarkMandelbrotBenchmark.benchMandelbrotRecursive8Thread",
    "SMarkNBody.benchNBody",
    "SMarkPolymorphyBenchmark.benchPolymorphy",
    "SMarkRSqueak.benchBitBltColorMapping",
    "SMarkRSqueak.benchDSAGen",
    "SMarkRSqueak.benchRenderFont",
    "SMarkRSqueak.benchShaLongString",
    "SMarkRichards.benchRichards",
    "SMarkShootout.benchShootoutBinarytrees",
    "SMarkShootout.benchShootoutChameneosRedux",
    "SMarkShootout.benchShootoutFannkuchRedux",
    "SMarkShootout.benchShootoutFasta",
    "SMarkShootout.benchShootoutFastaRedux",
    "SMarkShootout.benchShootoutKnucleotide",
    "SMarkShootout.benchShootoutMandelbrot3",
    "SMarkShootout.benchShootoutMeteor",
    "SMarkShootout.benchShootoutNBody",
    "SMarkShootout.benchShootoutPidigits",
    "SMarkShootout.benchShootoutRegexDNA",
    "SMarkShootout.benchShootoutReverseComplement",
    "SMarkShootout.benchShootoutSpectralNorm",
    "SMarkShootout.benchShootoutThreadring",
    "SMarkSlopstone.benchSlopstone",
    "SMarkSmopstone.benchSmopstone",
    "SMarkSplayTreeBenchmark.benchSplayTree",
    "SMarkSqueakJS.benchBitBltExampleOne",
    "SMarkSqueakJS.benchFib",
    "SMarkSqueakJS.benchFillArray",
    "SMarkSqueakJS.benchFillByteArray",
    "SMarkSqueakJS.benchFillString",
    "SMarkSqueakJS.benchIntegerBytecodes",
    "SMarkSqueakJS.benchMandala",
]

ITERATIONS = 100

OUTPUT_RE = re.compile(r"([a-zA-Z0-9]+) total: iterations=%s runtime: ([0-9\.]+)ms \+/\-([0-9\.]+)" % ITERATIONS)

from config import *
