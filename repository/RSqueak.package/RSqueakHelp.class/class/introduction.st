pages
introduction
	"This method was automatically generated. Edit it using:"
	"RSqueakHelp edit: #introduction"
	^(HelpTopic
		title: 'Welcome to RSqueak/VM'
		contents: 
'RSqueak/VM, an alternative virtual machine for Squeak/Smalltalk.

RSqueak/VM aims to be fully compatible with all Squeak versions, and currently supports Squeak versions from 2 to 5.1 with varying degrees of compatibility.

Why is RSqueak/VM made and how?

RSqueak/VM is a research VM. It is written in RPython, a variant of Python that is amendable to static analysis that grew out of the PyPy project. RPython can be translated to C automatically (much like Slang), but unlike Slang allows much more dynamic and high-level object-oriented code to be used. It also automatically adds features to the C code such as garbage collection, multi-threading, or software-transactional memory. It also automatically adds a capable tracing JIT, which even enables RSqueak/VM to surpass the standard Squeak VM (with Cog/Spur) on some benchmarks.

Why is the UI on RSqueak/VM so slow?

You may have noticed that startup and general UI interaction is slower on RSqueak/VM than on the Squeak VM. There are three (main) reasons for this:
    First, RSqueak/VM treats the image as an exchange format, rather than a memory format, and has to convert it into its internal representation during startup. This has the advantage that RSqueak/VM can support the different image formats that Squeak had over time.
    Second, RSqueak/VM does not have plugins for BitBlt and Balloon graphics, so all rendering that you see on the screen is entirely done in Smalltalk by running the Slang code included in VMMaker in pure Smalltalk. The fact the the image is useable at all is due to the aggressive optimizations the the JIT does to the Slang code, reaching somewhere between 5%-30% of native C performance for rendering.
    Third, RSqueak/VM''s JIT needs some time to warm up. You will notice that some actions are slower the first time, and get increasingly smoother. An extreme example is the "About Squeak" dialog: the first time it is opened, it may take up to 10 seconds to render. As you select other categories, you should notice that rendering speed improves, as does scrolling. After clicking around for a bit, try closing and re-opening the dialog. It should be faster now.

A nice effect of having all this rendering code run in Smalltalk is that you can always develop the entire graphics stack of Squeak in pure Smalltalk at runtime (rather than during VM development). Of course, if you break the code, there is often no way to recover, since the image can no longer render anything. The cost is in performance.

Why not just have a BitBlt plugin like the Squeak VM?

RSqueak/VM has significantly less code than the Squeak VM (even including the entire RPython translation toolchain). Much of this is due to the high-level nature of RPython compared to Slang. But a lot of the Squeak VM code is also in VM plugins. For RSqueak/VM, we have decided to only include VM plugins and primitives that add little code, easily map on to RPython libraries, or cannot be implemented in Squeak.

So what is this good for?

Not sure. Development in RSqueak/VM is much slower (and more unstable) than on Squeak VM. Our optimizations have been driven by our research goals, which were to see how fast we could get actual Smalltalk applications to run, based on what we consider is "common" and "idiomatic" code. Development time operations are very different, and include frequent changes to method dictionaries, classes, and instances, usages of "become", and the debugger writing to context objects. All these things are not very fast on RSqueak/VM.\

Right now, we think that RSqueak/VM is already useable if you are deploying an image that provides some service such as analysis of large amounts of data or number crunching. In these workloads, we usually expect RSqueak/VM to be as fast or faster than the Squeak VM. However, due to the nature of the project as a research vehicle, our focus is on discovering interesting architectures and optimizations for Smalltalk (and dynamic languages in general), as well teaching students about VMs. Stability and compatibility are important, but not crucial features for us. So if RSqueak/VM works for you and proves useful, we''d be happy to hear about it, but if there are bugs that you''d like us to fix, be patient: it may take us a while.

RSqueak/VM (doesn''t do what I want)/(is too slow on my benchmark)/(crashes all the time)!!

If you find problems, let us know by opening an issue. But please be patient with us and don''t complain if we cannot fix it in a few days - this is research and open source :)!
]style[(15 11 38 160 31 18 8 22 7 80 4 426 10 3 36 146 5 263 6 350 6 44 5 166 12 619 53 418 25 1266 11 1 79 37 16 122)b,f5b,b,f1,b,f1,Rhttps://github.com/hpi-swa/RSqueak;,f1,Rhttp://rpython.readthedocs.io/en/latest/faq.html#what-is-rpython;,f1,Rhttp://pypy.org/;,f1,Rhttp://speed.squeak.org/comparison/;,f1,b,f1,f1i,f1,f1i,f1,Rhttp://speed.squeak.org/timeline/#/?exe=2,1&ben=RenderFont&env=2&revs=50&equid=off&quarts=on&extr=on;,f1,f1i,f1,Rcode://SystemReporter open;,f1,b,f1,f1b,f1,f1b,f1,f1b,f1,Rhttps://github.com/hpi-swa/RSqueak/issues/new;,f1!' readStream nextChunkText)
			key: #introduction
