# -*- coding: utf-8 -*-
import os
import socket
import subprocess
import sys
import time
import urllib
import urllib2

SqueakImage = "Squeak4.4-12327"

# You need to enter the real URL and have the server running
CODESPEED_URL = 'http://speed.bithug.org/'

# Executables (assumed to be in the local directory)
executables = ["targetimageloadingsmalltalk-c"]

# Arguments (inserted between executable and benchmark)
executable_arguments = ["%s.image" % SqueakImage, "-m"]

# Benchmarks (run for each executable)
benchmarks = ["tinyBenchmarks"]

def build_data(executable, benchmark, result):
    # Mandatory fields
    return {
        'commitid': get_commitid(),
        'branch': 'default',
        'project': 'lang-smalltalk',
        'executable': executable,
        'benchmark': benchmark,
        'environment': socket.gethostname(),
        'result_value': str(result),
    }
    # Optional fields
    # data.update({
    #     'std_dev': 1.11111, # Optional. Default is blank
    #     'max': 4001.6, # Optional. Default is blank
    #     'min': 3995.1, # Optional. Default is blank
    # })


def download_prerequesites():
    clean_workspace()
    import gzip, tarfile
    image = urllib2.urlopen("http://ftp.squeak.org/4.4/%s.tgz" % SqueakImage).read()
    sources = urllib2.urlopen('http://ftp.squeak.org/4.4/SqueakV41.sources.gz').read()
    with open("image.tgz", "w") as f:
        f.write(image)
    f = gzip.open("image.tgz")
    tar = f.read()
    f.close()
    with open("image.tar", "w") as f:
        f.write(tar)
    f = tarfile.open("image.tar")
    f.extractall(".")
    f.close()
    with open("sources.gz", "w") as f:
        f.write(sources)
    f = gzip.open("sources.gz")
    with open("SqueakV41.sources", "w") as s:
        s.write(f.read())
    f.close()

def clean_workspace():
    for f in ["image.tgz", "image.tar", "sources.gz",
              "SqueakV41.sources",
              "%s.image" % SqueakImage, "%s.changes" % SqueakImage]:
        try:
            os.remove(f)
        except:
            pass


def get_commitid():
    try:
        pipe = subprocess.Popen(
            ["hg", "log", "-l", "1", "--template", "{node}"],
            stdout=subprocess.PIPE
        )
        if pipe.wait() == 0:
            return pipe.stdout.read()
    except:
        pass
    try:
        pipe = subprocess.Popen(
            ["git", "log", "-1", "--pretty=%H"],
            stdout=subprocess.PIPE
        )
        if pipe.wait() == 0:
            return pipe.stdout.read()
    except:
        pass
    raise Exception("commitid not found. not a git or hg repo")


def add(executable, benchmark, result):
    print "Saving result %s for executable %s, benchmark %s" % (
        result, executable, benchmark)
    data = build_data(executable, benchmark, result)
    params = urllib.urlencode(data)
    response = "None"
    print "Saving result for executable %s, revision %s, benchmark %s" % (
        data['executable'], data['commitid'], data['benchmark'])
    try:
        f = urllib2.urlopen(CODESPEED_URL + 'result/add/', params)
    except urllib2.HTTPError as e:
        print str(e)
        print e.read()
        return
    response = f.read()
    f.close()
    print "Server (%s) response: %s\n" % (CODESPEED_URL, response)


def run():
    suffix = ".exe" if sys.platform == "win32" else ""
    for executable in executables:
        for benchmark in benchmarks:
            start = time.time()
            pipe = subprocess.Popen(
                ["./%s%s" % (executable, suffix)] + executable_arguments + [benchmark]
            )
            pipe.wait()
            result = time.time() - start
            add(executable, benchmark, result)


if __name__ == "__main__":
    download_prerequesites()
    run()
