#!/usr/bin/env python
import BaseHTTPServer
import os
import signal
import subprocess
import sys

sys.path.insert(0, os.path.dirname(__file__))
from benchmark-queue import start as start_queue
from benchmark-where import start as start_worker
from constants import CONTROL_PORT


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
        return start_queue()
    WorkerPid = os.fork()
    if WorkerPid == 0:
        return start_worker()
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
    os.kill(QueuePid, signal.SIGTERM)
    os.kill(WorkerPid, signal.SIGTERM)
    print "Waiting for queue and worker to finish"
    os.wait(QueuePid)
    os.wait(WorkerPid)
    print "Updating from git"
    scriptdir = os.path.join(os.path.dirname(__file__), "scripts")
    pipe = subprocess.Popen("git pull", shell=True, cwd=scriptdir)
    pipe.wait()
    print "Updating image and Cog"
    os.system(os.path.join(scriptdir, "update_image"))
    os.system(os.path.join(scriptdir, "get_cog"))
    print "Exec self"
    os.execl(__file__)


if __name__ == "__main__":
    start()
