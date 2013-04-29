# -*- coding: utf-8 -*-
import os
import socket
import subprocess
import sys
import time
import urllib
import urllib2

SqueakImage = "Squeak4.5-12568"

# You need to enter the real URL and have the server running
CODESPEED_URL = 'http://speed.bithug.org/'

# Executables (assumed to be in the local directory)
executables = ["targetimageloadingsmalltalk-c", "coglinux/squeak"]

# Arguments (inserted between executable and benchmark)
executable_arguments = [["images/%s.image" % SqueakImage, '-m', 'runSPyBenchmarks'], ["images/%s.image" % SqueakImage, '../benchmarks.st']]

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
    # {
    #     'std_dev': 1.11111, # Optional. Default is blank
    #     'max': 4001.6, # Optional. Default is blank
    #     'min': 3995.1, # Optional. Default is blank
    # }


def ungzip(url, name, target):
    import gzip
    zip_contents = urllib2.urlopen(url).read()
    with open(name, "w") as f:
        f.write(zip_contents)
    f = gzip.open(name)
    with open(target, "w") as s:
        s.write(f.read())

def untar(name, target):
    import tarfile
    try:
        f = tarfile.open(name)
        f.extractall(target)
    finally:
        f.close()

def download_prerequesites():
    clean_workspace()
    print 'Downloading',
    download_cog()
    print 'done'

def download_cog():
    if sys.platform == "win32":
        url = "http://www.mirandabanda.org/files/Cog/VM/VM.r2714/cogwin-13.13.2714.zip"
        unzip(url, 'cogwin.zip', '.')
    else:
        url = "http://www.mirandabanda.org/files/Cog/VM/VM.r2714/coglinux-13.13.2714.tgz"
        print '.',
        ungzip(url, 'coglinux.tgz', 'coglinux.tar')
        print '.',
        untar('coglinux.tar', '.')

def clean_workspace():
    print 'Cleaning workspace',
    for f in ["image.tgz", "image.tar",
              'coglinux.tgz', 'coglinux.tar',
              'cogwin.zip']:
        try:
            os.remove(f)
        except:
            pass
        print '.',
    for d in ['coglinux', 'cogwin']:
        try:
            shutil.rmtree(d)
        except:
            pass
        print '.',
    print 'done'

def get_commitid():
    try:
        pipe = subprocess.Popen(
            ["hg", "log", "-l", "1", "--template", "{branch}-{rev} {date|shortdate}"],
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

def update_image(suffix):
    with open('update.st', 'w') as f:
        f.write('''Smalltalk snapshot: true andQuit: true.''')
    pipe = subprocess.Popen(
        ['cogmtlinux/squeak%s images/%s ../update.st' % (suffix, SqueakImage)],
        shell=True)
    pipe.wait()
    os.remove('update.st')


def run():
    suffix = ".exe" if sys.platform == "win32" else ""
    update_image(suffix)

    for i, executable in enumerate(executables):
        print 'Calling %s ...' % executable
        pipe = subprocess.Popen(
            ["./%s%s" % (executable, suffix)] + executable_arguments[i],
            stdout=subprocess.PIPE
        )
        out, err = pipe.communicate()
        errcode = pipe.wait()
        benchmarks = out.split('\n')
        print out
        for s in benchmarks:
            if ';' in s:
                name, time = s.split(';')
                add(executable, name, time)

if __name__ == "__main__":
    download_prerequesites()
    run()
