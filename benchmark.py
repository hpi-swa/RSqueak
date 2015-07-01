#!/usr/bin/env python

import os
import subprocess
import urllib
import urllib2
import re
import math
import shutil
import time
import sys

from optparse import OptionParser


# CODESPEED_URL = 'http://172.16.64.134/'

CODESPEED_URL = 'http://localhost:80/'
benchmarks = []


class Project(object):
    def __init__(self, name, executables=list(), arguments=list(), working_dir=""):
        self.name = name
        self.executables = executables
        self.arguments = arguments
        self.working_dir = working_dir

    def run(self):
        print "running executables: ", self.executables
        for executable in self.executables:
            for benchmark in benchmarks:
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

            results = self.parse_rsqueak_result(output)

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
        response = f.read()
        f.close()
        print "Server (%s) response: %s\n" % (CODESPEED_URL, response)

    def build_data(self, executable, benchmark, result, min, max, stdev):
        return {
            'commitid': full_commit_hash[0:10],
            'result_date': time.strftime("%Y-%m-%d %H:%M", time.localtime()),   # CodeSpeed doesn't really care ...
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
        regex = ".*#\(([0-9 ]+)\).*"
        trimmed_results = re.sub(regex, "\\1", result_string)
        results_list = trimmed_results.split(" ")

        results_list = [int(i) for i in results_list]
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
            ["%s" % self.path] + args,
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
            "bash",
        ),
    ],
    arguments=["-c",
               "SDL_VIDEODRIVER=dummy /home/dglaeser/RSqueakBenchmarks/RSqueak/rsqueak "
               "/home/dglaeser/RSqueakBenchmarks/Benchmark-Repo/Squeak4.6-vmmaker.bench.image -n 0"],
    working_dir="./RSqueak",
)

BENCHMARKS = {
    "mandala": {"rsqueak": "-m mandala"},
    "dsaGen": {"rsqueak": "-m dsaGen"},
    "shaLongString": {"rsqueak": "-m shaLongString"},
    # "renderFont": {"rsqueak": "-m renderFont"}, # does not work in headless mode
    "arrayFillArray": {"rsqueak": "-m arrayFillArray"},
    "arrayFillString": {"rsqueak": "-m arrayFillString"},
}

VMS = {
    "rsqueak": RSqueak
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


def get_commit_hash(required_commit):
    orig_dir = os.getcwd()

    repo_dir = "/home/dglaeser/RSqueakBenchmarks/RSqueak/"
    os.chdir(repo_dir)
    os.system("git fetch origin")
    if required_commit:
        try:
            subprocess.check_output("git checkout {}".format(required_commit),
                                    stderr=subprocess.STDOUT, cwd=repo_dir, shell=True)
        except subprocess.CalledProcessError:
            print "Required commit could not be found:", required_commit
            if len(required_commit) != 40:
                exit(-1)
    else:
        os.system("git checkout master")
        os.system("git pull")

    hash_long = subprocess.check_output("git rev-parse HEAD",
                                        stderr=subprocess.STDOUT, cwd=repo_dir, shell=True)
    os.chdir(orig_dir)
    return hash_long.strip()  # remove trailing whitespace from console output


def get_rsqueak_executable(required_commit, commit_hash):
    orig_dir = os.getcwd()
    os.chdir("/home/dglaeser/RSqueakBenchmarks/RSqueak/")
    if os.path.isfile("rsqueak"):
        os.remove("rsqueak")

    executable_name = get_executable_name(commit_hash, required_commit)

    cached = False
    if required_commit:
        cached = get_executable_from_cache(executable_name)

    if not cached:
        if check_executable_on_server(executable_name):
            # todo: error handling for web retrieval
            print "Retrieving executable from lively-kernel.org."
            urllib.urlretrieve("http://www.lively-kernel.org/babelsberg/RSqueak/{}".format(executable_name),
                               filename="rsqueak_builds/{}".format(executable_name))

    shutil.copy("rsqueak_builds/{}".format(executable_name), "rsqueak")
    os.chmod("rsqueak", 0755)  # executable by owner
    os.chdir(orig_dir)


def get_executable_name(commit_hash, required_commit):
    if required_commit:
        return "rsqueak-x86-Linux-jit-{}".format(commit_hash)
    else:
        return "rsqueak-linux-latest"


def check_executable_on_server(executable_name):
    try:
        urllib2.urlopen(HeadRequest("http://www.lively-kernel.org/babelsberg/RSqueak/{}".format(executable_name)))
    except Exception as e:
        print e
        print "Unable to retrieve executable."
        exit(-1)
    return True


def get_executable_from_cache(executable_name):
    if os.path.isfile("rsqueak_builds/{}".format(executable_name)):
        print "Cached executable found: {}".format(executable_name)
        return True
    else:
        print "No cached executable found: {}".format(executable_name)

    return False


if __name__ == "__main__":
    usage = "usage: %prog --benchmarks=Blowfist,Richards --vms=squeak,cog"
    parser = OptionParser(usage=usage)
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

    (options, args) = parser.parse_args()
    if options.list or (options.benchmarks is None and not options.benchmark_all) or (
            options.vms is None and not options.vm_all):
        print "Available Benchmarks:"
        for b in BENCHMARKS:
            print " *", b
        print "Available VMs:"
        for vm in VMS:
            print " *", vm
        parser.error("incorrect number of arguments")

    if options.logfile:
        sys.stdout = open("%s-%s-%s" % (options.logfile,
                                        time.strftime("%Y-%m-%d_%H-%M-%S", time.localtime()),  # time.time()
                                        options.required_commit), 'w')

    if options.benchmark_all:
        benchmarks = BENCHMARKS.keys()
    else:
        benchmarks = options.benchmarks.split(',')

    if options.vm_all:
        vms = VMS.keys()
    else:
        vms = options.vms.split(',')

    unknown_benchmarks = set(benchmarks).difference(set(BENCHMARKS.keys()))
    if unknown_benchmarks:
        print "Unknown Benchmarks: ", ', '.join(unknown_benchmarks)
        exit()

    unknown_vms = set(vms).difference(set(VMS.keys()))
    if unknown_vms:
        print "Unknown VMs: ", ', '.join(unknown_vms)
        exit()

    full_commit_hash = "hash-not-set"
    full_commit_hash = get_commit_hash(options.required_commit)

    get_rsqueak_executable(options.required_commit, full_commit_hash)

    for vm in vms:
        VMS[vm].post_results()

