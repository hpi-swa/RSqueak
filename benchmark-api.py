#!/usr/bin/env python
import BaseHTTPServer
import os
import urllib
import urllib2
import sys

sys.path.insert(0, os.path.dirname(__file__))
from constants import API_PORT, BENCHMARK_MACHINES, COMMITID, BRANCH, VM


class BenchmarkApi(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_POST(self):
        c = self.headers.getheader(COMMITID, None)
        b = self.headers.getheader(BRANCH, None)
        v = self.headers.getheader(VM, None)
        if not c or not b or not v: return
        for m in BENCHMARK_MACHINES:
            try:
                req = urllib2.Request(m)
                req.add_header(COMMITID, c)
                req.add_header(BRANCH, b)
                req.add_header(VM, v)
                req.add_data(COMMITID) # just to switch to POST
                f = urllib2.urlopen(req)
            except urllib2.HTTPError as e:
                print str(e)
                print e.read()
                return
            except urllib2.URLError as e:
                print str(e)
                return
            response = f.read()
            f.close()
            print "Machine %s response: %s\n" % (m, response)
        self.send_response(200)
        self.end_headers()


if __name__ == "__main__":
    print "Starting Benchmark API server"
    httpd = BaseHTTPServer.HTTPServer(('', API_PORT), BenchmarkApi)
    try:
        httpd.serve_forever()
    finally:
        print "Bye"
        httpd.server_close()
