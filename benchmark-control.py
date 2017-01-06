#!/usr/bin/env python
import BaseHTTPServer
import os
import signal
import subprocess
import sys
import time

sys.path.insert(0, os.path.dirname(__file__))
queue = __import__("benchmark-queue")
worker = __import__("benchmark-worker")
from constants import CONTROL_PORT, DBFILE, JOB_TABLE, FLAG, COMMITID, BENCHMARK_FIELD, VM, BRANCH


class BenchmarkControl(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path != '/selfupdate':
            self.send_response(404)
            self.end_headers()
        else:
            self.send_response(200)
            self.end_headers()
            selfupdate()


def start():
    global QueuePid
    global WorkerPid
    global httpd
    print "Starting Benchmark control"
    QueuePid = os.fork()
    if QueuePid == 0:
        return queue.start()
    time.sleep(2)
    WorkerPid = os.fork()
    if WorkerPid == 0:
        return worker.start()
    httpd = BaseHTTPServer.HTTPServer(('', CONTROL_PORT), BenchmarkControl)
    try:
        httpd.serve_forever()
    finally:
        print "Bye"
        httpd.server_close()


def selfupdate():
    print "Preparing self update"
    global QueuePid
    global WorkerPid
    global httpd
    httpd.server_close()
    os.kill(WorkerPid, signal.SIGTERM)
    print "Waiting for worker to finish"
    os.waitpid(WorkerPid, 0)
    print "Updating from git"
    scriptdir = os.path.join(os.path.abspath(os.path.dirname(__file__)), "scripts")
    pipe = subprocess.Popen("git checkout -- ../benchmarks.py", shell=True, cwd=scriptdir)
    pipe.wait()
    scriptdir = os.path.join(os.path.abspath(os.path.dirname(__file__)), "scripts")
    pipe = subprocess.Popen("git pull", shell=True, cwd=scriptdir)
    pipe.wait()
    print "Updating image and Cog VM"
    os.system(os.path.join(scriptdir, "get_cog.sh"))
    os.system(os.path.join(scriptdir, "get_cog64.sh"))
    os.system(os.path.join(scriptdir, "get_interpreter.sh"))
    os.system(os.path.join(scriptdir, "get_sista.sh"))
    os.system(os.path.join(scriptdir, "update_image.sh"))
    os.system(os.path.join(scriptdir, "get_sista_image.sh"))
    os.system(os.path.join(scriptdir, "update_benchmarks.sh"))
    print "Kill queue and re-exec self"
    os.kill(QueuePid, signal.SIGTERM)
    os.wait()
    os.execl(sys.executable, sys.executable, __file__)


if __name__ == "__main__":
    if len(sys.argv) <= 1:
        start()
    else:
        import sqlite3
        conn = sqlite3.connect(DBFILE)
        c = conn.cursor()
        if sys.argv[1] == "show":
            c.execute("SELECT * FROM %s WHERE %s=0;" % (JOB_TABLE, FLAG))
            print c.fetchall()
        elif sys.argv[1] == "history":
            c.execute("SELECT * FROM %s WHERE %s=1;" % (JOB_TABLE, FLAG))
            print c.fetchall()
        elif sys.argv[1] == "reset":
            commitid = sys.argv[2]
            print "Resetting benchmarks commit %s" % commitid
            c.execute("""
            UPDATE %s SET %s=0 WHERE %s='%s';
            """ % (JOB_TABLE, FLAG, COMMITID, commitid))
            c.execute("SELECT * FROM %s WHERE %s='%s';" % (JOB_TABLE, COMMITID, commitid))
            print c.fetchall()
            conn.commit()
        elif sys.argv[1] == "bench":
            vm = sys.argv[2]
            commitid = sys.argv[3]
            branch = sys.argv[4]
            try:
                benchmark = sys.argv[5]
                c.execute("""
                INSERT INTO %s (%s, %s, %s, %s, %s) VALUES ('%s', '%s', '%s', 0, '%s');
                """ % (JOB_TABLE, VM, COMMITID, BRANCH, FLAG, BENCHMARK_FIELD, vm, commitid, branch, benchmark))
            except IndexError:
                c.execute("""
                INSERT INTO %s (%s, %s, %s, %s) VALUES ('%s', '%s', '%s', 0);
                """ % (JOB_TABLE, VM, COMMITID, BRANCH, FLAG, vm, commitid, branch))
            conn.commit()
        else:
            commitid = sys.argv[1]
            print "Deleting benchmarks commit %s" % commitid
            c.execute("DELETE FROM %s WHERE %s='%s'" % (JOB_TABLE, COMMITID, commitid))
            conn.commit()
        c.close()
        conn.close()
