import os


def log(msg):
    if os.environ.get('DEBUG_LANG'):
        print msg
