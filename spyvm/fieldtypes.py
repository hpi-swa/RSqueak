from spyvm import model, shadow

from rpython.rlib import jit, signature

LPI = object()
int
float

object

maps = dict()

class VarSizedFieldTypes():
    _immutable_fields_ = []
    _attrs_ = []
    _settled_ = True

    @staticmethod
    def of_length(s_class, n):
        return nilTyper

    def __init__(self):
        pass

    def fetch(self, w_obj, n0):
        return w_obj._vars[n0]

    def store(self, w_obj, n0, w_val):
        w_obj._vars[n0] = w_val

class FieldTypes(VarSizedFieldTypes):
    _immutable_fields_ = ['types']
    _attrs_ = ['types', 'parent', 'siblings', 'diff']
    _settled_ = True

    def __init__(self, types, parent=None, change=(-1, object)):
        self.types = types
        self.parent = parent
        if parent is not None:
            assert change != (-1, object)
        self.diff = change

        self.siblings = dict()

    def fetch(self, w_object, n0):
        w_result = w_object._vars[n0]
        types = self.types
        if types[n0] is int:
            jit.record_known_class(w_result, model.W_SmallInteger)
        elif types[n0] is LPI:
            jit.record_known_class(w_result, model.W_LargePositiveInteger1Word)
        elif types[n0] is float:
            jit.record_known_class(w_result, model.W_Float)
        return w_result

    def store(self, w_object, n0, w_value):
        types = self.types
        changed_type = w_value.fieldtype()
        if types[n0] is not changed_type:
            w_object.fieldtypes = self.sibling(n0, changed_type)
        w_object._vars[n0] = w_value


    def sibling(self, n0, changed_type):
        assert self.types[n0] is not changed_type
        change = (n0, changed_type)
        parent = self.parent
        siblings = self.siblings
        if change in siblings:
            return siblings[change]
        elif parent is None:
            return self.descent([change])
        else:
            new_fieldtype = parent.ascent([change, self.diff])
            assert new_fieldtype.types == self.types[0:n0] + [changed_type] + self.types[n0+1:]
            siblings[change] = new_fieldtype
            return new_fieldtype

    def ascent(self, changes):
        parent = self.parent
        if parent is None:
            return self.descent(sorted(changes))
        else:
            change = self.diff
            if changes[0][0] != change[0]:
                changes.append(change)
            return parent.ascent(changes)

    def descent(self, changes):
        if changes == []:
            return self

        change = changes[0]
        siblings = self.siblings
        if change in siblings:
            return siblings[change].descent(changes[1:])
        else:
            new_types = list(self.types)
            new_types[change[0]] = change[1]
            new_fieldtype = FieldTypes(new_types, self, change)
            siblings[change] = new_fieldtype
            return new_fieldtype.descent(changes[1:])


    @staticmethod
    def of_length(n):
        if n not in maps:
            maps[n] = FieldTypes([object] * n)
        return maps[n]


nilTyper = VarSizedFieldTypes()
def fieldtypes_of_length(s_class, size):
    if s_class is None or s_class.isvariable():
        return nilTyper
    else:
        return FieldTypes.of_length(size)

def fieldtypes_of(w_obj):
    try:
        if w_obj.s_class.isvariable():
            return nilTyper
        else:
            vars = w_obj._vars
            size = len(vars)
            typer = FieldTypes.of_length(size)
            for i, w_val in enumerate(vars):
                changed_type = w_val.fieldtype()
                if changed_type is not object:
                    typer = typer.sibling(i, changed_type)
            return typer
    except AttributeError:
        return nilTyper