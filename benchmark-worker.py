#!/usr/bin/env python
import os
import signal
import socket
import sqlite3
import subprocess
import sys
import time
import urllib
import urllib2

sys.path.insert(0, os.path.dirname(__file__))
from constants import JOB_TABLE, COMMITID, FLAG, DBFILE, CODESPEED_URL, \
    BENCHMARKS, ITERATIONS, OUTPUT_RE, BINARY_URL, BINARY_BASENAME, BRANCH, VM

class BenchmarkWorker(object):
    def __init__(self):
        self.conn = sqlite3.connect(DBFILE)
        self.conn.row_factory = sqlite3.Row
        self.c = self.conn.cursor()
        self.should_terminate = False

    def serve_forever(self):
        while True:
            if self.should_terminate:
                print "Termination requested, done"
                return
            try:
                self.run()
                time.sleep(10)
            except Exception, e:
                print e
                time.sleep(10)

    def terminate(self):
        self.should_terminate = True

    def run(self):
        self.c.execute("""
        SELECT * FROM %s WHERE %s=0 LIMIT 1;
        """ % (JOB_TABLE, FLAG))
        result = self.c.fetchone()
        if result:
            commitid = result[COMMITID]
            branch = result[BRANCH]
            vm = result[VM]
            print "Running benchmarks for %s at %s on %s" % (vm, commitid, branch)
            self.c.execute("""
            UPDATE %s SET %s=1 WHERE %s='%s' AND %s='%s' AND %s='%s'
            """ % (JOB_TABLE, FLAG, COMMITID, commitid, BRANCH, branch, VM, vm))
            self.conn.commit()
            self.execute(vm, commitid, branch)

    def execute(self, vm, commitid, branch):
        if vm == "cog":
            binary = self.download_cog(commitid)
        elif vm == "rsqueak":
            binary = self.download_rsqueak(commitid)
        if not binary: return
        for bm in BENCHMARKS:
            with open("run.st", "w") as f:
                f.write("""
                [BenchmarkAutosizeSuite run: {
                'BenchmarkSimpleStatisticsReporter'.
                '%s'.
                %s}] on: Error do: ["Just fail"].
                [BenchmarkAutosizeSuite run: {
                'BenchmarkSimpleStatisticsReporter'.
                '%s'.
                %s}] on: Error do: ["Just fail"].
                Smalltalk image quitPrimitive.
                """ % (bm, ITERATIONS / 3, bm, ITERATIONS))
                f.flush()
            retries = 1
            while retries >= 0:
                try:
                    pipe = subprocess.Popen(
                        "%s $(pwd)/Squeak*.image $(pwd)/run.st" % binary,
                        shell=True,
                        stdout=subprocess.PIPE
                    )
                out, err = pipe.communicate()
                errcode = pipe.wait()
                if errcode == 0:
                    break
                else:
                    retries -= 1
                except Exception, e:
                    print e
            results = {}
            match = OUTPUT_RE.search(out)
            while match:
                # This purposefully overwrites any duplicate run result with the
                # second run, under the assumption that all previous were warmup
                results[match.group(1)] = (match.group(2), match.group(3))
                match = OUTPUT_RE.search(out, match.end(3))
            for b, r in results.iteritems():
                self.post_data(
                    vm=vm,
                    benchmark=b,
                    commitid=commitid,
                    branch=branch,
                    rtime=r[0],
                    stdev=r[1])

    def download_rsqueak(self, commitid):
        executable_name = BINARY_BASENAME.format(commitid)
        url = BINARY_URL.format(executable_name)
        print "Downloading %s" % url
        try:
            filename, _ = urllib.urlretrieve(url)
        except Exception, e:
            print e
            return None
        os.chmod(filename, 0755)
        return filename

    def download_cog(self, commitid):
        print "Downloading Cog"
        scriptdir = os.path.join(os.path.abspath(os.path.dirname(__file__)), "scripts")
        if os.system("%s %s" % (os.path.join(scriptdir, "get_cog.sh"), commitid)) != 0:
            return None
        else:
            return "./cogspurlinux/squeak"

    def post_data(self, vm=None, benchmark=None, commitid=None, branch=None, rtime=None, stdev=None):
        commit_date = time.strftime("%Y-%m-%d %H:%M", time.localtime())
        project = "cog" if "cog" in vm else "rsqueakvm"
        executable = "%s-%s" % (project, sys.platform)
        env = socket.gethostname()
        params = {
            'commitid': commitid[0:10],
            'result_date': commit_date,
            'branch': branch,
            'project': project,
            'executable': executable,
            'benchmark': benchmark,
            'environment': env,
            'result_value': rtime,
            'min': rtime,
            'max': rtime,
            'std_dev': stdev }
        params = urllib.urlencode(params)
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

    def __del__(self):
        self.conn.commit()
        self.c.close()
        self.conn.close()


def start():
    global worker
    worker = BenchmarkWorker()
    signal.signal(signal.SIGTERM, lambda signum, frame: worker.terminate())
    print "Running worker"
    worker.serve_forever()


if __name__ == "__main__":
    start()
