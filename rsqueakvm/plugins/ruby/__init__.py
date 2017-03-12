from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.util.cells import QuasiConstant

from topaz.objspace import ObjectSpace


ruby_space = ObjectSpace(None)

w_ruby_resume_method = QuasiConstant(None, type=W_CompiledMethod)
w_ruby_class = QuasiConstant(None, type=W_PointersObject)
w_ruby_object_class = QuasiConstant(None, type=W_PointersObject)
w_ruby_plugin_send = QuasiConstant(None, type=W_PointersObject)
