#! /usr/bin/env python

import sys, os

if __name__ == "__main__":
    import environment
    sys.argv.extend([
        "-T",
        os.path.join(os.path.dirname(__file__), "..", "images", "Squeak4.6-vmmaker.1.image")
    ])
    exec open(os.path.join(os.path.dirname(__file__), "..", "targetrsqueak.py")).read()
