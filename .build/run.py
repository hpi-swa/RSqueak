#! /usr/bin/env python

import sys, os

if __name__ == "__main__":
    import environment
    sys.argv.extend([
        "-T",
        os.path.join(os.path.dirname(__file__), "..", "images", "Squeak4.6-vmmaker.1.image")
    ])
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
    with open(os.path.join(os.path.dirname(__file__), "..", "targetrsqueak.py")) as f:
        exec f.read()
