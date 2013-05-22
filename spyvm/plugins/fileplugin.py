from spyvm import model, error
from spyvm.plugins.plugin import Plugin
from spyvm.primitives import PrimitiveFailedError, index1_0


FilePlugin = Plugin()

@FilePlugin.expose_primitive(unwrap_spec=[object])
def primitiveDirectoryDelimitor(interp, s_frame, w_rcvr):
    import os
    return interp.space.wrap_char(os.path.sep)

@FilePlugin.expose_primitive(unwrap_spec=[object, str, index1_0])
def primitiveDirectoryLookup(interp, s_frame, w_file_directory, full_path, index):
	import os
	print full_path
	if full_path == '':
		contents = os.listdir(os.path.sep)
	else:
		if not os.path.isdir(full_path):
			raise PrimitiveFailedError
		contents = os.listdir(full_path)
	space = interp.space
	if index >= len(contents):
		return space.w_nil
	name = sorted(contents)[index]
	file_path = os.path.join(full_path, name)
	assert os.path.exists(file_path)
	creationTime = space.wrap_int(0)
	modificationTime = space.wrap_int(0)
	dirFlag = space.w_true if os.path.isdir(file_path) else space.w_false
	fileSize = space.wrap_int(os.path.getsize(file_path))
	return space.wrap_list([space.wrap_string(name), creationTime, modificationTime, dirFlag, fileSize])

