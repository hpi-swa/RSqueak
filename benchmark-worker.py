#!/usr/bin/env python
import os
import platform
import socket
import sqlite3
import sys
import time
import urllib
import urllib2
from .constants import JOB_TABLE, COMMITID, FLAG, DBFILE, CODESPEED_URL, VMS, \
    BENCHMARKS, ITERATIONS, OUTPUT_RE

class BenchmarkWorker(object):
    def __init__(self):
        self.conn = sqlite3.connect(DBFILE)
        self.conn.row_factory = sqlite3.Row
        self.c = self.conn.cursor()

    def run(self):
        self.c.execute("""
        SELECT * FROM ? WHERE ?=0 LIMIT 1;
        """, (JOB_TABLE, FLAG))
        result = self.c.fetchone()
        if result:
            commitid = result[COMMITID]
            print "Running benchmarks for %s" % commitid
            self.execute(commitid)

    def execute(self, commitid):
        for bm in BENCHMARKS:
            with f as open("run.st", "w"):
                f.write("""
                BenchmarkAutosizeSuite run: {
                'BenchmarkSimpleStatisticsReporter'.
                '%s'.
                %s}.
                SmalltalkImage current snapshot: false andQuit: true.
                """ % (bm, ITERATIONS))
                for vm in VMS:
                    r = subprocess.check_output(
                        vm,
                        shell=True,
                        os.path.dirname(__file__)
                    )
                    match = OUTPUT_RE.search(r)
                    while match:
                        self.post_data(
                            vm=vm,
                            benchmark=match.group(1),
                            commitid=commitid,
                            time=int(match.group(2)),
                            stdev=int(match.group(3)))
                        match = OUTPUT_RE.search(r, match.end(3))

    def post_data(self, vm=None, benchmark=None, commitid=None, time=0, stdev=0):
        commit_date = time.strftime("%Y-%m-%d %H:%M", time.localtime())
        project = "rsqueakvm" if "rsqueak" in vm else "cog"
        executable = "%s-%s-%s" % (project, sys.platform, platform.architecture()[0])
        env = socket.gethostname()
        params = {
            'commitid': commitid[0:10],
            'result_date': commit_date,
            'branch': 'default',
            'project': project,
            'executable': executable,
            'benchmark': benchmark,
            'environment': env,
            'result_value': str(time),
            'min': str(time),
            'max': str(time),
            'std_dev': str(stdev) }
        params = urllib.urlencode(data)
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

if __name__ == "__main__":
    httpd = BaseHTTPServer.HTTPServer(('', 8082), BenchmarkQueue)
    try:
        httpd.serve_forever()
    finally:
        httpd.server_close()
