#!/usr/bin/env python
import BaseHTTPServer
import os
import sqlite3
import sys

sys.path.insert(0, os.path.dirname(__file__))
from constants import QUEUE_PORT, JOB_TABLE, COMMITID, FLAG, BRANCH, DBFILE, VM


class BenchmarkQueue(BaseHTTPServer.BaseHTTPRequestHandler):
    @staticmethod
    def ensure_db():
        conn = sqlite3.connect(DBFILE)
        c = conn.cursor()
        c.execute("""
        SELECT name FROM sqlite_master WHERE type='table' AND name='%s';
        """ % JOB_TABLE)
        if not c.fetchone():
            c.execute("""
            CREATE TABLE %s (%s VARCHAR(255), %s VARCHAR(255), %s VARCHAR(255), %s BYTE);
            """ % (JOB_TABLE, VM, COMMITID, BRANCH, FLAG))
            conn.commit()
        c.close()
        conn.close()

    def __init__(self, *args):
        self.conn = sqlite3.connect(DBFILE)
        self.c = self.conn.cursor()
        BaseHTTPServer.BaseHTTPRequestHandler.__init__(self, *args)

    def do_POST(self):
        commitid = self.headers.getheader(COMMITID)
        branch = self.headers.getheader(BRANCH)
        vm = self.headers.getheader(VM)
        self.c.execute("""
        SELECT * FROM %s WHERE %s='%s' AND %s='%s' LIMIT 1;
        """ % (JOB_TABLE, COMMITID, commitid, VM, vm))
        if self.c.fetchone():
            # already in DB, not running again
            self.send_response(304)
            self.end_headers()
        else:
            # insert the job
            self.c.execute("""
            INSERT INTO %s (%s, %s, %s, %s) VALUES ('%s', '%s', '%s', 0);
            """ % (JOB_TABLE, VM, COMMITID, BRANCH, FLAG, vm, commitid, branch))
            self.conn.commit()
            self.send_response(200)
            self.end_headers()

    def __del__(self):
        self.conn.commit()
        self.c.close()
        self.conn.close()


def start():
    print "Starting Benchmark queue"
    BenchmarkQueue.ensure_db()
    httpd = BaseHTTPServer.HTTPServer(('', QUEUE_PORT), BenchmarkQueue)
    try:
        httpd.serve_forever()
    finally:
        print "Bye"
        httpd.server_close()


if __name__ == "__main__":
    start()
