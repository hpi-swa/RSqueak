import sys, os

spyvm = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if spyvm not in sys.path:
    sys.path.insert(0, spyvm)
