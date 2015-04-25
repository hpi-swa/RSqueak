#! /usr/bin/env python

import sys, os, ConfigParser
from os.path import dirname, join as pathjoin


config = pathjoin(dirname(__file__), "buildconfig.ini")
cp = ConfigParser.ConfigParser()
if not os.path.exists(config):
    with open(config, 'w') as f:
        cp.add_section("General")
        cp.set("General", "pypy", pathjoin(dirname(__file__), "pypy"))
        cp.set("General", "rsdl", pathjoin(dirname(__file__), "rsdl"))
        cp.set("General", "squeak", pathjoin(dirname(__file__), "squeak"))
        cp.add_section("Windows")
        cp.set("Windows", "SDL", pathjoin(dirname(__file__), "SDL"))
        cp.set("Windows", "WindowsSDK7", "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A")
        cp.set("Windows", "VisualStudio9", "C:\\Program Files (x86)\\Microsoft Visual Studio 9.0")
        cp.set("Windows", "Graphviz", pathjoin(dirname(__file__), "Graphviz"))
        cp.write(f)
else:
    cp.read(config)

sys.path.insert(0, cp.get("General", "pypy"))
sys.path.insert(0, cp.get("General", "rsdl"))

try:
    import targetrsqueak as rsqueak
except ImportError:
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

if os.name == "nt":
    vs = cp.get("Windows", "VisualStudio9")
    sdk = cp.get("Windows", "WindowsSDK7")
    paths = [pathjoin(vs, "VC"), pathjoin(vs, "VC", "atlmfc"), sdk]
    os.environ['INCLUDE'] = ";".join([pathjoin(e, "include") for e in paths])
    os.environ['LIB'] = ";".join([pathjoin(e, "lib") for e in paths])
    os.environ['LIBPATH'] = ";".join([pathjoin(e, "lib") for e in paths[0:2]])
    os.environ['Path'] = ";".join([pathjoin(vs, "VC", "bin"),
                                   pathjoin(vs, "Common7", "IDE"),
                                   pathjoin(sdk, "Bin"),
                                   pathjoin(cp.get("Windows", "Graphviz"), "bin"),
                                   os.environ["Path"]])
    os.environ["SDL_PREFIX"] = cp.get("Windows", "SDL")
elif "linux" in sys.platform:
    pass
elif "darwin" == sys.platform:
    pass
else:
    raise AssertionError("Unsupported platform")
