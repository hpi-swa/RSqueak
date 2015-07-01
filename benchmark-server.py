#!/usr/bin/env python

import os
import BaseHTTPServer

class BenchmarkServer(BaseHTTPServer.BaseHTTPRequestHandler):
	def do_POST(self):
		print self.headers.__dict__
		commitid = self.headers.getheader('commitid', 'master')
		print "Running benchmarks for %s" % commitid
		if os.fork() == 0:
			os.execvp("python",
			["python", os.path.join(
				os.path.dirname(__file__),
				"benchmark.py"
			), "--vms=rsqueak", "--logfile=travis",
			"--benchmarks-all", "--commit-id=%s" % commitid])
		else:
			self.send_response(200)
			self.end_headers()

if __name__ == "__main__":
	httpd = BaseHTTPServer.HTTPServer(('', 8082), BenchmarkServer)
	try:
		httpd.serve_forever() 
	finally:
		httpd.server_close()

