#!/usr/bin/env python
import BaseHTTPServer
import sqlite3
from .constants import QUEUE_PORT, JOB_TABLE, COMMITID, FLAG, DBFILE


class BenchmarkQueue(BaseHTTPServer.BaseHTTPRequestHandler):
    def __init__(self, *args):
        super(*args)
        self.conn = sqlite3.connect(DBFILE)
        self.c = self.conn.cursor()
        self.c.execute("""
        SELECT name FROM sqlite_master WHERE type='table' AND name='?';
        """, (JOB_TABLE,))
        if not self.c.fetchone():
            self.c.execute("""
            CREATE TABLE ? (? VARCHAR(255), ? BYTE);
            """, (JOB_TABLE, COMMITID, FLAG))
            self.conn.commit()

    def do_POST(self):
        commitid = self.headers.getheader(COMMITID)
        self.c.execute("""
        SELECT * FROM ? WHERE ?='?' LIMIT 1;
        """, (job_table, COMMITID, commitid,))
        if self.c.fetchone():
            # already in DB, not running again
            self.send_response(304)
            self.end_headers()
        else:
            # insert the job
            self.c.execute("""
            INSERT INTO ? (?, ?) VALUES ('?', 0);
            """, (JOB_TABLE, COMMITID, FLAG, commitid,))
            self.conn.commit()
            self.send_response(200)
            self.end_headers()

    def __del__(self):
        self.conn.commit()
        self.c.close()
        self.conn.close()


if __name__ == "__main__":
    httpd = BaseHTTPServer.HTTPServer(('', QUEUE_PORT), BenchmarkQueue)
    try:
        httpd.serve_forever()
    finally:
        httpd.server_close()
