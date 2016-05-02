#!/usr/bin/env python
import os
import platform
import signal
import socket
import sqlite3
import subprocess
import sys
import time
import urllib
import urllib2

sys.path.insert(0, os.path.dirname(__file__))
from constants import JOB_TABLE, COMMITID, FLAG, DBFILE, CODESPEED_URL, VMS, \
    BENCHMARKS, ITERATIONS, OUTPUT_RE, BINARY_URL, BINARY_BASENAME, BRANCH

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
            print "Running benchmarks for %s on %s" % (commitid, branch)
            self.c.execute("""
            UPDATE %s SET %s=1 WHERE %s='%s' AND %s='%s'
            """ % (JOB_TABLE, FLAG, COMMITID, commitid, BRANCH, branch))
            self.conn.commit()
            self.execute(commitid, branch)

    def execute(self, commitid, branch):
        rsqueak = self.download_rsqueak(commitid)
        if not rsqueak:
            return
        for bm in BENCHMARKS:
            with open("run.st", "w") as f:
                f.write("""
                [BenchmarkAutosizeSuite run: {
                'BenchmarkSimpleStatisticsReporter'.
                '%s'.
                %s}] on: Error do: ["Just fail"].
                SmalltalkImage current snapshot: false andQuit: true.
                """ % (bm, ITERATIONS / 3, bm, ITERATIONS))
                f.flush()
            for vm in VMS:
                if "rsqueak" in vm: vm = rsqueak
                try:
                    pipe = subprocess.Popen(
                        "%s $(pwd)/Squeak*.image $(pwd)/run.st" % vm,
                        shell=True,
                        stdout=subprocess.PIPE
                    )
                except Exception, e:
                    print e
                    break # don't bother executing the other VM if we cannot
                          # compare
                out, err = pipe.communicate()
                errcode = pipe.wait()
                match = OUTPUT_RE.search(out)
                while match:
                    self.post_data(
                        vm=vm,
                        benchmark=match.group(1),
                        commitid=commitid,
                        branch=branch,
                        rtime=match.group(2),
                        stdev=match.group(3))
                    match = OUTPUT_RE.search(out, match.end(3))

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

    def post_data(self, vm=None, benchmark=None, commitid=None, branch=None, rtime=None, stdev=None):
        commit_date = time.strftime("%Y-%m-%d %H:%M", time.localtime())
        project = "cog" if "cog" in vm else "rsqueakvm"
        executable = "%s-%s-%s" % (project, sys.platform, platform.architecture()[0])
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
