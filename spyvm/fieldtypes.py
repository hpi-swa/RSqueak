from spyvm import model, shadow

from rpython.rlib import objectmodel, jit, signature
from rpython.rlib.listsort import TimSort

class TypeTag():
    pass

LPI = TypeTag()
SInt = TypeTag()
flt = TypeTag()
obj = TypeTag()

# may be used during debugging
# LPI, SInt, flt, obj = 'LPI', 'SInt', 'float', 'object'

class FieldSort(TimSort):
    def lt(self, a, b):
        return a[0] < b[0]

maps = {}

class AbstractFieldTypes():
    _immutable_fields_ = []
    _attrs_ = []
    _settled_ = True

    def __init__(self):
        pass
    def fetch(self, w_obj, n0):
        raise NotImplementedError("Abstract base class")
    def store(self, w_obj, n0, w_val):
        raise NotImplementedError("Abstract base class")
    def initial_storage(self, size):
        raise NotImplementedError("Abstract base class")
    def storage_for(self, collection):
        raise NotImplementedError("Abstract base class")

# This is the regular storage strategy that does not result in any
# optimizations but can handle every case. Applicable for both
# fixed-sized and var-sized objects.
class ListStorageStrategy(AbstractFieldTypes):
    def fetch(self, w_obj, n0):
        return w_obj._vars[n0]
    def store(self, w_obj, n0, w_val):
        w_obj._vars[n0] = w_val
    def initial_storage(self, size):
        return [nil_obj] * size
    def storage_for(self, collection):
        return [x for x in collection]
ListStorageStrategy.singleton = ListStorageStrategy()

class FixedSizeFieldTypes(AbstractFieldTypes):
    _immutable_fields_ = ['types[*]']
    _attrs_ = ['types', 'parent', 'siblings', 'diff']
    _settled_ = True

    def __init__(self, types, parent=None, change=(-1, obj)):
        self.types = types
        self.parent = parent
        if parent is not None:
            assert change != (-1, obj)
        self.diff = change
        self.siblings = {}
    
    def initial_storage(self, size):
        return [w_nil] * size
    
    def storage_for(self, collection):
        return [x for x in collection]
    
    def fetch(self, w_object, n0):
        w_result = w_object._vars[n0]
        assert w_result is not None
        types = self.types
        # TODO - try 'assert isinstance' instead.
        if types[n0] is SInt:
            jit.record_known_class(w_result, model.W_SmallInteger)
        elif types[n0] is LPI:
            jit.record_known_class(w_result, model.W_LargePositiveInteger1Word)
        elif types[n0] is flt:
            jit.record_known_class(w_result, model.W_Float)
        return w_result

    def store(self, w_object, n0, w_value):
        types = self.types
        changed_type = w_value.fieldtype()
        if types[n0] is not changed_type:
            w_object.fieldtypes = self.sibling(n0, changed_type)
        w_object._vars[n0] = w_value

    @jit.elidable
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
            if n0 == self.diff[0]:
                diff = [change]
            else:
                diff = [change, self.diff]

            new_fieldtype = parent.ascent(diff)

            if not objectmodel.we_are_translated():
                new_types = list(self.types)
                new_types[n0] = changed_type
                assert new_fieldtype.types == new_types
            siblings[change] = new_fieldtype
            return new_fieldtype

    def ascent(self, changes):
        parent = self.parent
        if parent is None:
            FieldSort(changes).sort()
            return self.descent(changes)
        else:
            change = self.diff
            if changes[0][0] != change[0]:
                changes.append(change)
            return parent.ascent(changes)

    def descent(self, changes):
        if changes == []:
            return self

        change = changes[0]
        if change[1] is obj:
            return self.descent(changes[1:])
        siblings = self.siblings
        if change in siblings:
            return siblings[change].descent(changes[1:])
        else:
            new_types = list(self.types)
            assert new_types[change[0]] == obj
            new_types[change[0]] = change[1]
            new_fieldtype = FixedSizeFieldTypes(new_types, self, change)
            siblings[change] = new_fieldtype
            return new_fieldtype.descent(changes[1:])

    @staticmethod
    @jit.elidable
    def of_length(n):
        if n not in maps:
            maps[n] = FixedSizeFieldTypes([obj] * n)
        return maps[n]

def strategy_for(w_obj, size):
    try:
        if w_obj.s_class.isvariable():
            return ListStorageStrategy.singleton
        else:
            vars = w_obj._vars
            typer = FixedSizeFieldTypes.of_length(size)
            for i, w_val in enumerate(vars):
                changed_type = w_val.fieldtype()
                if changed_type is not obj:
                    typer = typer.sibling(i, changed_type)
            return typer
    except AttributeError:
        return ListStorageStrategy.singleton
