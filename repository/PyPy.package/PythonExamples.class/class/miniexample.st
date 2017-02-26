debugging
miniexample
	"Use user interrupt to interrupt while loop"
	^ Python pexec: '
x = 0

def start():
    global x
    while True:
        x += 1
        if x % 1000 == 0:
            print x
start()'