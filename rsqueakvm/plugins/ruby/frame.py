from topaz.module import ClassDef
from topaz.objects.objectobject import W_BaseObject as WR_BaseObject


class WR_FrameObject(WR_BaseObject):
    _attrs_ = ['frame_object']
    _immutable_fields_ = ['frame_object']

    classdef = ClassDef('FrameObject', WR_BaseObject.classdef)

    def __init__(self, frame_object):
        self.frame_object = frame_object

    @classdef.method('get_previous')
    def method_get_previous(self, space):
        previous = self.frame_object.backref()
        if previous is not None:
            return WR_FrameObject(previous)
        return space.w_nil

    @classdef.method('has_contents')
    def method_has_contents(self, space):
        return space.newbool(self.frame_object.has_contents())

    @classdef.method('get_filename')
    def method_get_filename(self, space):
        return space.newstr_fromstr(self.frame_object.get_filename())

    @classdef.method('get_lineno')
    def method_get_lineno(self, space, prev_wr_frame=None):
        prev_frame = None
        if prev_wr_frame is not None:
            if not isinstance(prev_wr_frame, WR_FrameObject):
                return space.w_nil
            prev_frame = prev_wr_frame.frame_object
        return space.newstr_fromstr(self.frame_object.get_lineno(prev_frame))

    @classdef.method('get_code_name')
    def method_get_code_name(self, space):
        return space.newstr_fromstr(self.frame_object.get_code_name())
