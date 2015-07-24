#!/usr/bin/env python

import os
import BaseHTTPServer
import subprocess
import fcntl
import time


class BenchmarkServer(BaseHTTPServer.BaseHTTPRequestHandler):

    def do_POST(self):
        print self.headers.__dict__
        commitid = self.headers.getheader('commitid', 'master')
        queue = "commit-queue"

        self.update_commit_queue(commitid, queue)

        # Todo: the forked benchmark processes turn into zombies - could be prevented
        # either by double-forking or regularly collecting (un)dead children
        benchmark_pid = os.fork()
        if benchmark_pid == 0:
            self.run_benchmarks(commitid)
            os._exit(0)
        else:
            print "Send http response 200."
            self.send_response(200)
            self.end_headers()

    def update_commit_queue(self, commitid, queue_file):
        if not os.path.isfile(queue_file):  # ensure file exists
            file(queue_file, 'w').close()
        with open(queue_file, 'r+') as fd:
            print "Locking commit queue ..."
            self.wait_for_lock(fd)

            commits = fd.readlines()
            commits.append(commitid + '\n')

            fd.seek(0)
            fd.writelines(commits)

    def wait_for_lock(self, fd):
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except IOError as e:
            print "Wait for lock ..."
            time.sleep(2)
            self.wait_for_lock(fd)

    def run_benchmarks(self, commitid):
        print "Running benchmarks for %s" % commitid

        subprocess.Popen(["python",
                          os.path.join(os.path.dirname(__file__), "benchmark.py"),
                          "--vms=rsqueak", "--logfile=travis",
                          "--benchmarks-all", "--continue"])

if __name__ == "__main__":
    httpd = BaseHTTPServer.HTTPServer(('', 8082), BenchmarkServer)
    try:
        httpd.serve_forever()
    finally:
        httpd.server_close()

