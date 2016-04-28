#!/usr/bin/env python
import sqlite3
from .constants import JOB_TABLE, COMMITID, FLAG, DBFILE, CODESPEED_URL,

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

    def execute(self):


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
