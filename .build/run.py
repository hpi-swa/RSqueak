#! /usr/bin/env python

import sys, os

if __name__ == "__main__":
    import environment
    exec open(os.path.join(os.path.dirname(__file__), "..", "targetrsqueak.py")).read()
