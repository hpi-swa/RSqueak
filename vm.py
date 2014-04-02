#! /usr/bin/python

# --- RPYTHON/CHROOT TOOLCHAIN HELPER ---
#
#     This script restarts itself inside the 32-bit chroot environment
#     and runs either the translation or a benchmark.
#     (!) Double-check that everything uses absolute paths (!)
#

import os
import sys

###############################################################################
# --- CHROOT ENVIRONMENT ---

PASSWD  = 'bot'
CHROOT  = '/var/chroot/precise'


IN_CHROOT = 'echo %(PASSWD)s | sudo -S chroot %(CHROOT)s' % locals()
CHROOT_INDICATOR = '--chrooted' # option to indicate switch

# Command for restarting this script inside chroot:

ABS_PATH = os.path.abspath(sys.argv[0]) # b/c relative paths explode!
CHROOT_SELF = IN_CHROOT \
              + ' python ' \
              + ' "%s" ' % ABS_PATH \
              + " ".join(['"%s"' % a for a in sys.argv[1:]]) \
              + ' ' + CHROOT_INDICATOR


###############################################################################
# --- TRANSLATE ---

RPYTHON = '~/.local/bin/rpython'
#OPTIONS = '-O2 --gcrootfinder=shadowstack'  # ...as Lars told me
OPTIONS = ' -O2 --gcrootfinder=stm --gc=stmgc --thread'
TARGET  = '~/lang-smalltalk/targetimageloadingsmalltalk.py'


TRANSLATE = '%(RPYTHON)s %(OPTIONS)s %(TARGET)s' % locals()

###############################################################################
# --- BENCHMARKS ---

BENCHMARKS = {
    'fact50': {
        'image': '~/lang-smalltalk/images/Squeak4.5-12568.image',
        'method': 'factorial',
        'integer': 50
    },

    'fib50': {
        'image': '~/lang-smalltalk/images/Squeak4.5-12568.image',
        'method': 'benchFib',
        'integer': 50
    },


    'fib': {
        'image': '~/lang-smalltalk/images/Squeak4.5-12568.image',
        'method': 'benchFib',
    },

    'stm': {
        'image': '~/lang-smalltalk/images/Squeak4.5-12568.image',
        'method': 'benchStm',
        'integer': 8
    }
}


EXECUTABLE = 'SDL_VIDEODRIVER=dummy '\
             + '~/lang-smalltalk/targetimageloadingsmalltalk-c'
RUN = EXECUTABLE + ' %(image)s -m %(method)s -n %(integer)s'

###############################################################################
# --- SCRIPT ---
def usage():
    print('Please specify one of the following options:\n\ttranslate\t - RPython Translation')
    for k in BENCHMARKS:
        if 'integer' in BENCHMARKS[k]:
            print('\t%s\t\t - Benchmark' % k)
        else:
            print('\t%s <n>\t\t - Benchmark with argument n' % k)

if len(sys.argv) < 2:
    usage()

else:
    # We are in chroot, just call stuff
    if CHROOT_INDICATOR in sys.argv:

        if sys.argv[1] in ('translate', 't'):
            print(TRANSLATE)
            os.system(TRANSLATE)

        elif sys.argv[1] in BENCHMARKS:
            benchmark = BENCHMARKS[sys.argv[1]]

            # Read integer from argument if not fixed
            if not 'integer' in benchmark:
                benchmark['integer'] = int(sys.argv[2])

            print(RUN % benchmark)
            os.system(RUN % benchmark)

        else:
            usage()

    # we are not in chroot, restart this script inside chroot
    else:
        print("*** Entering chroot environment ***")
        print(CHROOT_SELF)
        os.system(CHROOT_SELF)


