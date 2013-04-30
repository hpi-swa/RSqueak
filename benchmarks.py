# -*- coding: utf-8 -*-
import os
import shutil
import socket
import subprocess
import sys
import time
import urllib
import urllib2

SqueakImage = "Squeak4.5-12568"

# You need to enter the real URL and have the server running
CODESPEED_URL = 'http://speed.bithug.org/'


class Project(object):
    def __init__(self, name, executables={}, arguments="", commitid=None):
        self.commitid = commitid if commitid else self.get_commitid()
        self.name = name
        self.executables = executables
        self.arguments = arguments

    def run(self):
        for executable in self.executables:
            yield executable.name, executable.run(self.arguments)

    def post_results(self):
        for executable, output in self.run():
            benchmarks = output.split('\n')
            for s in benchmarks:
                if ';' in s:
                    name, time = s.split(';')
                    self.add(executable, name, time)

    def add(self, executable, benchmark, result):
        print "Saving result %s for executable %s, benchmark %s" % (
            result, executable, benchmark)
        data = self.build_data(executable, benchmark, result)
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

    def get_commitid(self):
        try:
            pipe = subprocess.Popen(
                ["hg", "log", "-l", "1", "--template", "{rev}:{node}"],
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

    def build_data(self, executable, benchmark, result):
        # Mandatory fields
        return {
            'commitid': self.commitid,
            'branch': 'default',
            'project': self.name,
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


class Archive(object):
    def __init__(self, filename, target, func):
        self.filename = filename
        self.func = func
        self.target = target

    def extract(self):
        self.func(self.filename, self.target)

    def __enter__(self):
        self.extract()

    def __exit__(self, *_):
        if os.path.exists(self.target) and os.path.isfile(self.target):
            os.remove(self.target)


class Executable(object):
    def __init__(self, name, path, url=None, callback=None):
        self.name = name
        self.path = path
        if url:
            self.download(url, callback=callback)

    def ungzip(self, source, target):
        import gzip
        contents = gzip.open(source).read()
        with open(target, "w") as t:
            t.write(contents)

    def untar(self, source, target):
        import tarfile
        try:
            f = tarfile.open(source)
            f.extractall(target)
        finally:
            f.close()

    def download(self, url, callback=None):
        if os.path.exists(self.path):
            shutil.rmtree(os.path.dirname(self.path))
        filename = url.rsplit("/", 1)[1]
        if os.path.exists(filename):
            os.remove(filename)
        print "Downloading from", url
        with open(filename, "w") as f:
            f.write(urllib2.urlopen(url).read())
        try:
            print "Extracting", filename
            if filename.endswith(".tar.gz") or filename.endswith(".tgz"):
                tarfile = os.path.basename(filename) + ".tar"
                with Archive(filename, tarfile, self.ungzip):
                    Archive(tarfile, ".", self.untar).extract()
            elif filename.endswith(".tar"):
                Archive(filename, ".", self.untar).extract()
            else:
                raise NotImplementedError
        finally:
            os.remove(filename)
        if callback:
            callback(self)

    def run(self, args):
        print 'Calling %s ...' % executable
        pipe = subprocess.Popen(
            ["%s" % executable.path] + args,
            stdout=subprocess.PIPE
        )
        out, err = pipe.communicate()
        errcode = pipe.wait()
        print out
        return out


# XXX: Find a better place to put this
def update_image(executable):
    print "Updating image ..."
    with open('update.st', 'w') as f:
        f.write('''Smalltalk snapshot: true andQuit: true.''')
    print executable.run(["-vm-display-X11", "-headless", "images/%s" % SqueakImage, "../update.st"])
    os.remove('update.st')


def find_cog_url():
    baseurl = "http://www.mirandabanda.org/files/Cog/VM/"
    r = urllib2.urlopen(baseurl)
    ver = r.read().rsplit("VM.r", 1)[1].split("/", 1)[0]
    vmfolder = "%s/VM.r%s/" % (baseurl, ver)
    r = urllib2.urlopen(vmfolder).read()
    off = r.find("coglinux")
    filename = r[off:r.find(".tgz", off)] + ".tgz"
    return ver, vmfolder + filename
cogid, cogurl = find_cog_url()


Cog = Project(
    "squeak",
    executables=[
        Executable(
            "cogvm",
            "coglinux/squeak",
            cogurl,
            callback=update_image
        )
    ],
    arguments=['-vm-display-X11', '-headless', "images/%s.image" % SqueakImage, '../benchmarks.st'],
    commitid=cogid
)
RSqueakVM = Project(
    "lang-smalltalk",
    executables=[
        Executable("targetimageloadingsmalltalk-c", "./targetimageloadingsmalltalk-c")
    ],
    arguments=["images/%s.image" % SqueakImage, '-m', 'runSPyBenchmarks']
)


if __name__ == "__main__":
    for project in [Cog, RSqueakVM]:
        project.post_results()
