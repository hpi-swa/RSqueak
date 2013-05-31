# struct VirtualMachine* sqGetInterpreterProxy(void);

# typedef struct VirtualMachine {
# 	sqInt (*minorVersion)(void);
# } VirtualMachine;

# Loading a Plugin:
# 	plugin setInterpreter: proxy.
# 	(plugin respondsTo: #initialiseModule) ifTrue:[plugin initialiseModule].
# 	plugin perform: primitiveName asSymbol.
from spyvm import error

from rpython.rlib.entrypoint import entrypoint
from rpython.rtyper.annlowlevel import llhelper
from rpython.rlib.exports import export_struct
from rpython.rtyper.lltypesystem.lltype import FuncType, Struct, Ptr
from rpython.rtyper.lltypesystem import lltype

sqInt = lltype.Signed
sqLong = lltype.SignedLongLong

minorVFTP = Ptr(FuncType([], sqInt))

VirtualMachine = Struct("VirtualMachine",
					("minorVersion", minorVFTP)
				)
VMPtr = Ptr(VirtualMachine)
# export_struct("VirtualMachine", VirtualMachine)

@entrypoint('main', [], c_name='sqGetInterpreterProxy')
def sqGetInterpreterProxy():
	if not InterpreterProxy.vm_initialized:
		vm_proxy = lltype.malloc(VirtualMachine, flavor='raw')
		vm_proxy.minorVersion = llhelper(minorVFTP, minorVersion)
		InterpreterProxy.vm_proxy = vm_proxy
		InterpreterProxy.vm_initialized = True
	return InterpreterProxy.vm_proxy

def minorVersion():
	return 1


class _InterpreterProxy(object):
	_immutable_fields_ = ['vm_initialized?']

	def __init__(self):
		self.vm_proxy = lltype.nullptr(VMPtr.TO)
		self.vm_initialized = False

	def call(self, signature, interp, s_frame, argcount, s_method):
		print "Hello World..."
		raise error.Exit("External Call")

InterpreterProxy = _InterpreterProxy()
