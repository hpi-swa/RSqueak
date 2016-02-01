#!/usr/bin/env python
from collections import deque

import os
import subprocess
import urllib
import urllib2
import re
import math
import shutil
import time
import sys
import fcntl

from optparse import OptionParser


# CODESPEED_URL = 'http://172.16.64.134/'

THIS_DIR = os.path.abspath(os.path.dirname(__file__))
CODESPEED_URL = 'http://localhost:80/'
BINARY_URL = "https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/{}"
COMMIT_QUEUE = 'commit-queue'
COG_URL = "http://www.mirandabanda.org/files/Cog/VM/"
SYNC_FILE = os.path.join(os.getcwd(), "bm.lock")
input_benchmarks = []
commit_date = time.strftime("%Y-%m-%d %H:%M", time.localtime())

class Project(object):
    def __init__(self, name, executables=list(), arguments=list(), working_dir=""):
        self.name = name
        self.executables = executables
        self.arguments = arguments
        self.working_dir = working_dir

    def run(self):
        print "running executables: ", self.executables
        for executable in self.executables:
            for benchmark in input_benchmarks:
                yield executable.name, executable.run(self.arguments, benchmark, self.working_dir), benchmark

    def print_results(self):
        for executable, output in self.run():
            print "%s: %s" % (executable, output)

    def post_results(self):
        for executable, output, benchmark in self.run():
            print "posting..."
            print "executable: ", executable
            print "output: ", output
            print "current bm: ", benchmark

            try:
                results = self.parse_rsqueak_result(output)
            except ValueError, e:
                print(e)
                return

            self.add(executable, benchmark, median(results), min(results), max(results),
                     sample_standard_deviation(results))

    def add(self, executable, benchmark, result, min, max, stdev):
        print "Saving result %s for executable %s, benchmark %s" % (result, executable, benchmark)
        data = self.build_data(executable, benchmark, result, min, max, stdev)
        params = urllib.urlencode(data)

        print "Saving result for executable %s, revision %s, data %s" % (
            data['executable'], data['commitid'], data)
        try:
            f = urllib2.urlopen(CODESPEED_URL + 'result/add/', params)
        except urllib2.HTTPError as e:
            print str(e)
            print e.read()
            return
        except urllib2.URLError as e:
            print str(e)
            return
        response = f.read()
        f.close()
        print "Server (%s) response: %s\n" % (CODESPEED_URL, response)

    def build_data(self, executable, benchmark, result, min, max, stdev):
        return {
            'commitid': full_commit_hash[0:10],
            'result_date': commit_date,
            'branch': 'default',
            'project': self.name,
            'executable': executable,
            'benchmark': benchmark,
            'environment': 'Maxinchen',
            'result_value': str(result),
            'min': str(min),
            'max': str(max),
            'std_dev': str(stdev),
        }

    @staticmethod
    def parse_rsqueak_result(result_string):
        regex = r"<W_BytesObject.*'#\(([0-9eE\.\-\+ ]+)\)'>"
        trimmed_results = re.findall(regex, result_string)[0]
        results_list = trimmed_results.strip().split(" ")

        results_list = [float(i) for i in results_list]
        relevant_results = results_list[-20:]

        return relevant_results


class Executable(object):
    def __init__(self, name, path):
        self.name = name
        self.path = path

    def run(self, args_, benchmark, working_dir):
        args = args_[:]
        args[-1] = args[-1] + " " + BENCHMARKS[benchmark]["rsqueak"]
        print 'Calling %s (%s) ...' % (self.name, " ".join([self.path] + args))

        pipe = subprocess.Popen(
            " ".join([self.path] + args),
            shell=True,
            stdout=subprocess.PIPE,
            env={"SDL_VIDEODRIVER": "dummy"},
            cwd=working_dir
        )
        print "subprocess opened"
        out, err = pipe.communicate()
        errcode = pipe.wait()
        print "errcode: ", errcode

        return out

class CogExecutable(Executable):
    def run(self, args, benchmark, working_dir):
        with open(working_dir + "/startupscript.st", "w") as f:
            f.write("""
              | o |
              o := 0 %s.
              FileStream stdout nextPutAll: 'running benchmark'; crlf; flush;
                                nextPutAll: '<W_BytesObject ''';
                                nextPutAll: o asString;
                                nextPutAll: '''>'; cr; cr;
                                flush.
              Smalltalk snapshot: false andQuit: true.
            """ % BENCHMARKS[benchmark]["rsqueak"].split(" ")[-1])
        print 'Calling %s (%s) ...' % (self.name, " ".join([self.path] + args))
        pipe = subprocess.Popen(
            " ".join([self.path] + args),
            shell=True,
            stdout=subprocess.PIPE,
            cwd=working_dir
        )
        print "subprocess opened"
        out, err = pipe.communicate()
        errcode = pipe.wait()
        print "errcode: ", errcode
        return out

class HeadRequest(urllib2.Request):
    """Helper class to allow HEAD request before downloading executable."""
    def get_method(self):
        return "HEAD"

RSqueak = Project(
    "rsqueakvm",
    executables=[
        Executable(
            "rsqueakvm",
            "%s/RSqueak/rsqueak" % THIS_DIR,
        ),
    ],
    arguments=["%s/Squeak4.6-vmmaker.bench.image -n 0" % THIS_DIR],
    working_dir="./RSqueak",
)

Cog = Project(
    "cog",
    executables=[
        CogExecutable(
            "cog",
            "%s/coglinux/bin/squeak" % THIS_DIR
        ),
    ],
    arguments=["-headless %s/Squeak4.6-vmmaker.bench.image startupscript.st" % THIS_DIR],
    working_dir=".",
)
BENCHMARKS = {
    "mandala": {"rsqueak": "-m mandala"},
    "dsaGen": {"rsqueak": "-m dsaGen"},
    "shaLongString": {"rsqueak": "-m shaLongString"},
    # NOT WORKING: "renderFont": {"rsqueak": "-P -u -m renderFont"},
    "arrayFillArray": {"rsqueak": "-m arrayFillArray"},
    "arrayFillString": {"rsqueak": "-m arrayFillString"},
}

for b in [
          'SMarkAStarBenchmarkbenchAStar',
          'SMarkBinaryTreeBenchmarkbenchBinaryTree',
          'SMarkBlowfishBenchmarkbenchBlowfish',
          'SMarkCompilerbenchCompiler',
          'SMarkFannkuchbenchFannkuch',
          'SMarkFannkuchbenchmark',
          'SMarkJSJsonbenchJson',
          'SMarkLoopsbenchArrayAccess',
          'SMarkLoopsbenchSendWithManyArguments',
          'SMarkLoopsbenchFloatLoop',
          'SMarkLoopsbenchSend',
          'SMarkLoopsbenchClassVarBinding',
          'SMarkLoopsbenchIntLoop', 'SMarkLoopsbenchInstVarAccess',
          'SMarkMailinglistMicroBenchmarksbenchByteStringHash',
          'SMarkMailinglistMicroBenchmarksbenchOrderedCollectionRandomInsert',
          'SMarkMailinglistMicroBenchmarksbenchOrderedCollectionInsertFirst',
          'SMarkMailinglistMicroBenchmarksbenchWideStringHash',
          'SMarkMandelbrotbenchmark',
          'SMarkNBodybenchmark',
          'SMarkRichardsbenchRichards',
          'SMarkSlopstonebenchStone',
          'SMarkSmopstonebenchStone',
          'SMarkSplayTreeBenchmarkbenchSplayTree',
          'SMarkSqueakJSbenchmarkIntegerBench',
          'SMarkSqueakJSbenchmarkBitBltExampleOne',
          'SMarkSqueakJSbenchmarkFillByteArray',
          'SMarkSqueakJSbenchmarkFib']:
    name = re.sub(r"(...+?)\1+",
                  "\\1",
                  re.sub(r"bench(.)",
                         "\\1",
                         re.sub("smark|squeakjs|benchmark|mailinglistmicrobenchmarks",
                                "",
                                b,
                                flags=re.I
                         )
                        ),
                  flags=re.I
           )
    while BENCHMARKS.get(name, None) is not None:
        name += "_i"
    BENCHMARKS[name] = {"rsqueak": "-m " + b}

for b in [# 'SMarkMailinglistMicroBenchmarksbenchLRUCachePrintString',
          # 'SMarkGSGraphSearchbenchGraphSearch',
          # 'SMarkDeltaBluebenchDeltaBlue',
          'SMarkChameneosBenchmarkbenchChameneos',
          'SMarkMandelbrotBenchmarkbenchMandelbrotRecursive2Thread',
          'SMarkMandelbrotBenchmarkbenchMandelbrotRecursive8Thread',
          'SMarkMandelbrotBenchmarkbenchMandelbrotIterative2Thread',
          'SMarkMandelbrotBenchmarkbenchMandelbrotIterative4Thread',
          'SMarkMandelbrotBenchmarkbenchMandelbrotRecursive4Thread',
          'SMarkMandelbrotBenchmarkbenchMandelbrotIterative1Thread',
          'SMarkMandelbrotBenchmarkbenchMandelbrotRecursive1Thread',
          'SMarkMandelbrotBenchmarkbenchMandelbrotIterative8Thread',
          # 'SMarkPolymorphyBenchmarkbenchPolymorphy',
          'SMarkRSqueakbenchBitBlt']:
    name = re.sub(r"(...+?)\1+",
                  "\\1",
                  re.sub(r"bench(.)",
                         "\\1",
                         re.sub("smark|squeakjs|benchmark|mailinglistmicrobenchmarks",
                                "",
                                b,
                                flags=re.I
                         )
                        ),
                  flags=re.I
           )
    while BENCHMARKS.get(name, None) is not None:
        name += "_i"
    BENCHMARKS[name] = {"rsqueak": "-P -m " + b}


VMS = {
    "rsqueak": RSqueak,
    "cog": Cog
}


def mean(values):
    return sum(values) / float(len(values))


def median(values):
    m, remainder = divmod(len(values), 2)
    values.sort()
    if remainder:
        return values[m]
    else:
        return sum(values[m - 1:m + 1]) / 2


def sample_standard_deviation(values):
    deviation_sum = 0
    for v in values:
        deviation_sum += (mean(values) - v) ** 2
    return math.sqrt(deviation_sum / float((len(values) - 1)))


def get_full_hash_from_repo(required_commit):
    orig_dir = os.getcwd()

    repo_dir = "%s/RSqueak/" % THIS_DIR
    os.chdir(repo_dir)
    os.system("git fetch origin")
    if required_commit:
        try:
            subprocess.check_output("git checkout {}".format(required_commit),
                                    stderr=subprocess.STDOUT, cwd=repo_dir, shell=True)
        except subprocess.CalledProcessError:
            print "Required commit could not be found in git history:", required_commit
            if len(required_commit) != 40:
                raise CommitNotFoundException
    else:
        os.system("git checkout master")
        os.system("git pull")

    global commit_date
    commit_date = subprocess.check_output("git log -n 1 --pretty=format:%ct",
                                 stderr=subprocess.STDOUT, cwd=repo_dir, shell=True)
    commit_date = time.strftime("%Y-%m-%d %H:%M", time.gmtime(int(commit_date.strip())))
    hash_long = subprocess.check_output("git rev-parse HEAD",
                                        stderr=subprocess.STDOUT, cwd=repo_dir, shell=True)
    os.chdir(orig_dir)
    return hash_long.strip()  # remove trailing whitespace from console output


def update_cog():
    folder, archive = "", ""
    f = urllib.urlopen(COG_URL)
    regex = r"href=\"(VM.r[0-9]+)"
    links = re.findall(regex, f.read())
    links.sort()
    folder = links[-1]
    f.close()
    f = urllib.urlopen("%s/%s/" % (COG_URL, folder))
    regex = r"href=\"(coglinux[0-9\.\-]+tgz)"
    links = re.findall(regex, f.read())
    links.sort()
    archive = links[-1]
    f.close()
    filename, headers = urllib.urlretrieve("%s/%s/%s" % (COG_URL, folder, archive))
    import tarfile, shutil
    shutil.rmtree("%s/coglinux" % THIS_DIR, ignore_errors=True)
    with tarfile.open(filename) as tar:
        tar.extractall()
        topdir = tar.getnames()[0]
        print "Found cog at %s, moving to %s" % (topdir, "%s/coglinux" % THIS_DIR)
        shutil.move(topdir, "%s/coglinux" % THIS_DIR)


def get_rsqueak_executable(options, commit_hash):
    orig_dir = os.getcwd()
    os.chdir("%s/RSqueak/" % THIS_DIR)
    if os.path.isfile("rsqueak"):
        os.remove("rsqueak")
    executable_name = get_executable_name(commit_hash, options)

    cached = False
    # if options.required_commit or options.continue_queue:
    cached = get_executable_from_cache(executable_name)

    if not cached:
        if check_executable_on_server(executable_name):
            # todo: error handling for web retrieval
            print "Retrieving executable from lively-kernel.org."
            urllib.urlretrieve(BINARY_URL.format(executable_name),
                               filename=("%s/rsqueak_builds/%s" % (THIS_DIR, executable_name)))

    shutil.copy("%s/rsqueak_builds/%s" % (THIS_DIR, executable_name), "rsqueak")
    os.chmod("rsqueak", 0755)  # executable by owner
    os.chdir(orig_dir)


def get_executable_name(commit_hash, options):
    if options.required_commit or options.continue_queue:
        return "rsqueak-x86-linux-jit-{}".format(commit_hash)
    else:
        return "rsqueak-linux-latest"

def check_executable_on_server(executable_name):
    try:
        urllib2.urlopen(HeadRequest(BINARY_URL.format(executable_name)))
    except Exception as e:
        print BINARY_URL.format(executable_name)
        print e
        print "Unable to retrieve executable."
        raise MissingExecutableException
    return True


def get_executable_from_cache(executable_name):
    if os.path.isfile("%s/rsqueak_builds/%s" % (THIS_DIR, executable_name)):
        print "Cached executable found: {}".format(executable_name)
        return True
    else:
        print "No cached executable found: {}".format(executable_name)

    return False


def add_cmd_line_options(parser):
    parser.add_option("--benchmarks-all", action="store_true", dest="benchmark_all",
                      help="Run all benchmarks")
    parser.add_option("--vm-all", action="store_true", dest="vm_all",
                      help="Run all benchmarks")
    parser.add_option("-b", "--benchmarks", dest="benchmarks",
                      help="List of BENCHMARKS to run")
    parser.add_option("-v", "--vms", dest="vms",
                      help="List of VMs to run")
    parser.add_option("-l", "--list", action="store_true", dest="list",
                      help="List available VMs und BENCHMARKs")
    parser.add_option("--commit-id", dest="required_commit",
                      help="Commit that will be benchmarked. At least 7 digits.")
    parser.add_option("--logfile", dest="logfile",
                      help="Write stdout to file.")
    parser.add_option("--continue", action="store_true", dest="continue_queue",
                      help="Continue to benchmark commits stored in queue file.")


def evaluate_options(options):
    global VMS, BENCHMARKS

    if options.list or (options.benchmarks is None and not options.benchmark_all) \
            or (options.vms is None and not options.vm_all):
        print "Available Benchmarks:"
        for b in BENCHMARKS:
            print " *", b
        print "Available VMs:"
        for vm in VMS:
            print " *", vm
        option_parser.error("incorrect number of arguments")

    if options.benchmark_all:
        _benchmarks = BENCHMARKS.keys()
    else:
        _benchmarks = options.benchmarks.split(',')
    if options.vm_all:
        _vms = VMS.keys()
    else:
        _vms = options.vms.split(',')

    return _vms, _benchmarks


def reject_unknown_input(vms, benchmarks):
    unknown_benchmarks = set(benchmarks).difference(set(BENCHMARKS.keys()))
    if unknown_benchmarks:
        print "Unknown Benchmarks: ", ', '.join(unknown_benchmarks)
        exit()
    unknown_vms = set(vms).difference(set(VMS.keys()))
    if unknown_vms:
        print "Unknown VMs: ", ', '.join(unknown_vms)
        exit()


def ensure_isolated_benchmark_execution():
    print "Test if lock file {} exists.".format(SYNC_FILE)
    if os.path.isfile(SYNC_FILE):
        print "Benchmarks script already running, since file '{}' exists. Exit.".format(SYNC_FILE)
        sys.exit(0)
    else:
        open(SYNC_FILE, 'w').close()
        return SYNC_FILE


def wait_for_lock(fd):
    try:
        fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except IOError:
        print "Wait for lock on {} ...".format(fd.name)
        time.sleep(2)
        wait_for_lock(fd)


def get_next_queued_commit():
    with open(COMMIT_QUEUE, "r+") as fd:
        wait_for_lock(fd)
        commits = deque(fd.readlines())
        if len(commits) == 0:
            return None

        next_hash = commits.popleft().strip()
        print "Next hash: ", next_hash
        print "Remaining commits in queue: ", commits

        fd.seek(0)
        fd.truncate(0)

        fd.writelines(commits)
        return next_hash


def delete_sync_file(filename):
    print "Deleting lock file. {}".format(filename)
    if os.path.isfile(filename):
        os.remove(filename)
        print "{} deleted.".format(filename)


class MissingExecutableException(Exception):
    pass


class CommitNotFoundException(Exception):
    pass


if __name__ == "__main__":
    try:
        usage = "usage: %prog --benchmarks=mandala,shaLongString --vms=rsqueak"
        option_parser = OptionParser(usage=usage)
        add_cmd_line_options(option_parser)
        (option_values, args) = option_parser.parse_args()
        input_vms, input_benchmarks = evaluate_options(option_values)

        reject_unknown_input(input_vms, input_benchmarks)

        ensure_isolated_benchmark_execution()

        print "Starting benchmark routine for given commits."
        commits_enqueued = True
        while commits_enqueued:
            full_commit_hash = "hash-not-set"
            if option_values.continue_queue:
                full_commit_hash = get_next_queued_commit()
                if full_commit_hash is None:
                    break
            else:
                full_commit_hash = get_full_hash_from_repo(option_values.required_commit)
                commits_enqueued = False

            if option_values.logfile:
                sys.stdout = open("%s-%s-%s" % (option_values.logfile,
                                                time.strftime("%Y-%m-%d_%H-%M-%S", time.localtime()),
                                                full_commit_hash), 'w')

            get_rsqueak_executable(option_values, full_commit_hash)
            update_cog()

            for vm in input_vms:
                VMS[vm].post_results()

    except Exception:
        # This does (deliberately) not cover SystemExit or KeyboardInterrupt but all unexpected exceptions
        delete_sync_file(SYNC_FILE)
        raise

    delete_sync_file(SYNC_FILE)
