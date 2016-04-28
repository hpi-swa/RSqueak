#!/usr/bin/env python
import BaseHTTPServer
import os
import urllib
import urllib2
import sys

sys.path.insert(0, os.path.dirname(__file__))
from constants import API_PORT, BENCHMARK_MACHINES


class BenchmarkApi(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_POST(self):
        params = urllib.urlencode({
            'commitid': self.headers.getheader('commitid', 'master')
        })
        for m in BENCHMARK_MACHINES:
            try:
                f = urllib2.urlopen(m, params)
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


if __name__ == "__main__":
    print "Starting Benchmark API server"
    httpd = BaseHTTPServer.HTTPServer(('', API_PORT), BenchmarkApi)
    try:
        httpd.serve_forever()
    finally:
        print "Bye"
        httpd.server_close()
